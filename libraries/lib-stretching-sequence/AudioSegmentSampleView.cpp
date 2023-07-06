/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentSampleView.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioSegmentSampleView.h"

AudioSegmentSampleView::AudioSegmentSampleView(
   std::vector<BlockSampleView> blockViews, size_t start, sampleCount length)
    : mBlockViews { std::move(blockViews) }
    , mStart { start }
    , mLength { length }
    , mIsSilent { false }
{
}

AudioSegmentSampleView::AudioSegmentSampleView(sampleCount length)
    : mLength { length }
    , mIsSilent { true }
{
}

size_t AudioSegmentSampleView::Copy(float* buffer, size_t bufferSize) const
{
   if (!mIsSilent)
      return DoCopy(buffer, bufferSize);
   const auto numOutputSamples = limitSampleBufferSize(bufferSize, mLength);
   std::fill(buffer, buffer + numOutputSamples, 0.f);
   return numOutputSamples;
}

sampleCount AudioSegmentSampleView::GetSampleCount() const
{
   return mLength;
}

size_t AudioSegmentSampleView::DoCopy(float* buffer, size_t bufferSize) const
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
   return written;
}

void FillBufferFromTrackBlockSequence(
   const ChannelSampleView& track, float* buffer, size_t bufferSize)
{
   size_t written = 0;
   for (const auto& segment : track)
   {
      size_t toWrite =
         limitSampleBufferSize(bufferSize - written, segment.GetSampleCount());
      written += segment.Copy(buffer + written, toWrite);
   }
   std::fill(buffer + written, buffer + bufferSize, 0.f);
}
