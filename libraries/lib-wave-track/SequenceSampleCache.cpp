#include "SequenceSampleCache.h"

#include <cassert>

SequenceSampleCache::SequenceSampleCache(
   SourceSampleGetterFn fn, sampleCount sequenceLength, size_t maxBlockSize)
    : mGetterFn(std::move(fn))
    , mMaxBlockSize(maxBlockSize)
    , mSequenceLength(sequenceLength)
{
}

bool SequenceSampleCache::Get(
   samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
   bool mayThrow)
{
   auto containingBlockIt = std::find_if(
      mBlocks.begin(), mBlocks.end(),
      [start](const std::optional<Block>& block) {
         return block.has_value() && block->Contains(start);
      });
   if (containingBlockIt == mBlocks.end())
   {
      const auto blockIndex =
         mLastContainingBlockIndex.has_value() ?
            (*mLastContainingBlockIndex + 1) % mBlocks.size() :
            0u;
      const auto numSamplesToCache =
         std::min(sampleCount { mMaxBlockSize }, mSequenceLength - start)
            .as_size_t();
      auto& block = mBlocks[blockIndex];
      block.emplace(numSamplesToCache, format, start);
      if (!mGetterFn(
             block->buffer.ptr(), format, start, numSamplesToCache, mayThrow))
      {
         assert(false);
         std::fill(
            block->buffer.ptr(),
            block->buffer.ptr() + numSamplesToCache * SAMPLE_SIZE(format), 0);
      }
      mLastContainingBlockIndex = blockIndex;
      containingBlockIt = mBlocks.begin() + blockIndex;
   }
   const auto& block = *containingBlockIt;
   const auto sampleOffset = (start - block->sequenceStartSample).as_size_t();
   const auto numSamplesToRead =
      std::min(len, block->numSamples - sampleOffset);
   const auto numBytesToRead = numSamplesToRead * SAMPLE_SIZE(format);
   const auto blockPtr =
      block->buffer.ptr() + sampleOffset * SAMPLE_SIZE(format);
   std::copy(blockPtr, blockPtr + numBytesToRead, buffer);
   if (numSamplesToRead == len)
   {
      return true;
   }
   else
   {
      return Get(
         buffer + numBytesToRead, format, start + numSamplesToRead,
         len - numSamplesToRead, mayThrow);
   }
}

SequenceSampleCache::Block::Block(
   size_t numSamples, sampleFormat format, sampleCount sequenceStartSample)
    : format(format)
    , sequenceStartSample(sequenceStartSample)
    , numSamples(numSamples)
    , buffer(numSamples, format)
{
}

bool SequenceSampleCache::Block::Contains(sampleCount sample) const
{
   return sequenceStartSample <= sample &&
          sample < sequenceStartSample + numSamples;
}
