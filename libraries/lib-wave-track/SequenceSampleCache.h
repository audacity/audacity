#pragma once

#include "SampleCount.h"
#include "SampleFormat.h"

#include <array>
#include <functional>
#include <optional>

class SequenceSampleCache
{
public:
   using SourceSampleGetterFn = std::function<bool(
      samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
      bool mayThrow)>;

   SequenceSampleCache(
      SourceSampleGetterFn fn, sampleCount sequenceLength, size_t maxBlockSize);

   bool Get(
      samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
      bool mayThrow);

private:
   struct Block
   {
      Block(size_t numSamples, sampleFormat format, sampleCount sequenceStartSample);
      bool Contains(sampleCount) const;
      const sampleFormat format;
      const sampleCount sequenceStartSample;
      const size_t numSamples;
      SampleBuffer buffer;
   };

   const SourceSampleGetterFn mGetterFn;
   const sampleCount mSequenceLength;
   const size_t mMaxBlockSize;
   std::array<std::optional<Block>, 2> mBlocks;
   std::optional<size_t> mLastContainingBlockIndex;
};
