/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentSampleView.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioSegmentSampleView.h"

#include <cassert>
#include <numeric>

AudioSegmentSampleView::AudioSegmentSampleView(
   std::vector<BlockSampleView> blockViews, size_t start, size_t length)
    : mBlockViews { std::move(blockViews) }
    , mStart { start }
    , mLength { length }
    , mIsSilent { false }
{
   assert(
      start + length <=
      std::accumulate(
         mBlockViews.begin(), mBlockViews.end(), 0u,
         [](size_t acc, const auto& block) { return acc + block->size(); }));
}

AudioSegmentSampleView::AudioSegmentSampleView(size_t length)
    : mLength { length }
    , mIsSilent { true }
{
}

void AudioSegmentSampleView::Copy(float* buffer, size_t bufferSize) const
{
   mIsSilent ? std::fill(buffer, buffer + bufferSize, 0.f) :
               DoCopy(buffer, bufferSize);
}

size_t AudioSegmentSampleView::GetSampleCount() const
{
   return mLength;
}

void AudioSegmentSampleView::DoCopy(float* buffer, size_t bufferSize) const
{
   size_t toWrite { limitSampleBufferSize(bufferSize, mLength) };
   size_t written = 0u;
   size_t offset = mStart;
   for (const auto& block : mBlockViews)
   {
      const auto toWriteFromBlock = std::min(block->size() - offset, toWrite);
      std::copy(
         block->data() + offset, block->data() + offset + toWriteFromBlock,
         buffer + written);
      toWrite -= toWriteFromBlock;
      written += toWriteFromBlock;
      offset = 0;
   }
   std::fill(buffer + written, buffer + bufferSize, 0.f);
}
