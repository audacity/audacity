/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FloatVectorClip.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "FloatVectorClip.h"
#include "AudioSegmentSampleView.h"
#include <cmath>

FloatVectorClip::FloatVectorClip(
   int sampleRate, const std::vector<std::vector<float>>& audio)
    : mSampleRate { sampleRate }
    , mAudio { audio }
{
}

namespace
{
std::vector<std::vector<float>>
Duplicate(const std::vector<float>& audio, size_t numChannels)
{
   std::vector<std::vector<float>> duplicate;
   for (auto i = 0u; i < numChannels; ++i)
      duplicate.push_back(audio);
   return duplicate;
}
} // namespace

FloatVectorClip::FloatVectorClip(
   int sampleRate, const std::vector<float>& audio, size_t numChannels)
    : FloatVectorClip(sampleRate, Duplicate(audio, numChannels))
{
}

AudioSegmentSampleView FloatVectorClip::GetSampleView(
   size_t iChannel, sampleCount start, size_t len, bool mayThrow) const
{
   std::vector<BlockSampleView> blockViews {
      std::make_shared<std::vector<float>>(mAudio[iChannel])
   };
   // todo(mhodgkinson) review argument types.
   return AudioSegmentSampleView(
      std::move(blockViews), start.as_size_t(), len);
}

sampleCount FloatVectorClip::GetVisibleSampleCount() const
{
   return mAudio[0].size();
}

size_t FloatVectorClip::GetWidth() const
{
   return mAudio.size();
}

int FloatVectorClip::GetRate() const
{
   return mSampleRate;
}

sampleCount FloatVectorClip::TimeToSamples(double time) const
{
   return sampleCount(floor(time * GetRate() + 0.5));
}

double FloatVectorClip::GetPlayDuration() const
{
   return stretchRatio * mAudio[0].size() / mSampleRate;
}
