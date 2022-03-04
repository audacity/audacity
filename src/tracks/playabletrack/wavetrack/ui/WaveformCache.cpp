/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveformCache.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "WaveformCache.h"

#include <cmath>
#include <cstring>
#include <algorithm>

#include "Sequence.h"
#include "SampleBlock.h"

#include "GetWaveDisplay.h"
#include "WaveClipUtilities.h"
#include "PixelSampleMapper.h"

#include "ZoomInfo.h"

#include "waveform/WaveDataCache.h"


//
// Getting high-level data from the track for screen display and
// clipping calculations
//
template<size_t blockSize>
void FillBlocksFromAppendBuffer(
   samplePtr buffer, size_t samplesCount, WaveCacheSampleBlock& outBlock)
{
   const float* samples =
      static_cast<const float*>(static_cast<const void*>(buffer));

   const size_t blocksCount = RoundUp(samplesCount, blockSize);

   const size_t offset = RoundUp(outBlock.NumSamples, blockSize) * 3;

   float* outBuffer = static_cast<float*>(outBlock.GetWritePointer(
                         sizeof(float) * (offset + 3 * blocksCount))) +
                      offset;

   for (size_t blockIndex = 0; blockIndex < blocksCount; ++blockIndex)
   {
      const size_t samplesInBlock = std::min(blockSize, samplesCount);

      samplesCount -= samplesInBlock;

      float min = std::numeric_limits<float>::infinity();
      float max = -std::numeric_limits<float>::infinity();

      double sqSum = 0.0;

      for (size_t sampleIndex = 0; sampleIndex < samplesInBlock; ++sampleIndex)
      {
         const float sample = *samples++;

         min = std::min(min, sample);
         max = std::max(max, sample);

         const double dbl = sample;

         sqSum += dbl * dbl;
      }

      *outBuffer++ = min;
      *outBuffer++ = max;
      *outBuffer++ = static_cast<float>(std::sqrt(sqSum / samplesInBlock));
   }
}

bool FillFromAppendBuffer(
      const WaveClip& clip, WaveCacheSampleBlock& outBlock)
   {
   const size_t appendedSamples = clip.GetAppendBufferLen();
   
   if (appendedSamples == 0)
      return false;

   samplePtr appendBuffer = clip.GetAppendBuffer().ptr();

   switch (outBlock.DataType)
   {
   case WaveCacheSampleBlock::Type::Samples:
   {
      float* outBuffer =
         static_cast<float*>(outBlock.GetWritePointer(
            sizeof(float) * (outBlock.NumSamples + appendedSamples))) +
         outBlock.NumSamples;

      std::copy(appendBuffer, appendBuffer + appendedSamples, outBuffer);
   }
   break;
   case WaveCacheSampleBlock::Type::MinMaxRMS256:
      FillBlocksFromAppendBuffer<256>(appendBuffer, appendedSamples, outBlock);
   break;
   case WaveCacheSampleBlock::Type::MinMaxRMS64k:
      FillBlocksFromAppendBuffer<64 * 1024>(appendBuffer, appendedSamples, outBlock);
   break;
   default:
      return false;
   }

   outBlock.NumSamples += appendedSamples;

   return true;
}

bool WaveClipWaveformCache::GetWaveDisplay(
   const WaveClip &clip, WaveDisplay &display, double t0,
   double pixelsPerSecond )
{
   t0 += clip.GetTrimLeft();

   const bool allocated = display.mapper.IsValid();

   const size_t numPixels = (int)display.width;

   if (!mWaveDataCache)
   {
      mWaveDataCache = std::make_unique<WaveDataCache>(
         [sequence = clip.GetSequence(), rate = clip.GetRate(), clip = &clip](
            int64_t requiredSample, WaveCacheSampleBlock::Type dataType,
            WaveCacheSampleBlock& outBlock)
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
               outBlock.NumSamples = 0;

               return FillFromAppendBuffer(*clip, outBlock);
            }

            const auto blockIndex = sequence->FindBlock(requiredSample);
            const auto& inputBlock = sequence->GetBlockArray()[blockIndex];

            outBlock.FirstSample = inputBlock.start.as_long_long();
            outBlock.NumSamples = inputBlock.sb->GetSampleCount();
            
            switch (dataType)
            {
            case WaveCacheSampleBlock::Type::Samples:
            {
               samplePtr ptr = static_cast<samplePtr>(outBlock.GetWritePointer(
                  outBlock.NumSamples * sizeof(float)));

               inputBlock.sb->GetSamples(
                  ptr, floatSample, 0, outBlock.NumSamples, false);
            }
            break;
            case WaveCacheSampleBlock::Type::MinMaxRMS256:
            {
               size_t framesCount = RoundUp(outBlock.NumSamples, 256);

               float* ptr = static_cast<float*>(
                  outBlock.GetWritePointer(framesCount * sizeof(float) * 3));

               inputBlock.sb->GetSummary256(ptr, 0, framesCount);
            }
            break;
            case WaveCacheSampleBlock::Type::MinMaxRMS64k:
            {
               size_t framesCount = RoundUp(outBlock.NumSamples, 64 * 1024);

               float* ptr = static_cast<float*>(
                  outBlock.GetWritePointer(framesCount * sizeof(float) * 3));

               inputBlock.sb->GetSummary256(ptr, 0, framesCount);
            }
            break;
            default:
               return false;
            }

            if ((blockIndex + 1) == sequence->GetBlockArray().size())
            {
               FillFromAppendBuffer(*clip, outBlock);
            }

            return true;
         },
         clip.GetRate());
   }

   if (!allocated)
      display.Allocate();

   auto range = mWaveDataCache->PerformLookup(
      ZoomInfo(0.0f, pixelsPerSecond), t0, t0 + numPixels / pixelsPerSecond);

   for (auto it = range.begin(); it != range.end(); ++it)
   {
      const auto leftOffset = it.GetLeftOffset();
      const auto rightOffset = GraphicsDataCacheBase::CacheElementWidth - it.GetRightOffset();

      const WaveDisplayColumn* firstColumn = it->Data.data();

      display.AppendColumns(
         it->Data.data() + leftOffset,
         it->Data.data() + rightOffset);
   }
  
   return !range.empty();
}

WaveClipWaveformCache::WaveClipWaveformCache()
{
}

WaveClipWaveformCache::~WaveClipWaveformCache()
{
}

static WaveClip::Caches::RegisteredFactory sKeyW{ []( WaveClip& ){
   return std::make_unique< WaveClipWaveformCache >();
} };

WaveClipWaveformCache &WaveClipWaveformCache::Get( const WaveClip &clip )
{
   return const_cast< WaveClip& >( clip ) // Consider it mutable data
      .Caches::Get< WaveClipWaveformCache >( sKeyW );
}

void WaveClipWaveformCache::MarkChanged()
{
   ++mDirty;
}

void WaveClipWaveformCache::Invalidate()
{
   if (mWaveDataCache)
      mWaveDataCache->Invalidate();
}
