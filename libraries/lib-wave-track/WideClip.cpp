/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WideClip.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "WideClip.h"

WideClip::WideClip(
   std::shared_ptr<ClipInterface> left, std::shared_ptr<ClipInterface> right)
    : mChannels { std::move(left), std::move(right) }
{
}

AudioSegmentSampleView WideClip::GetSampleView(
   size_t ii, sampleCount start, size_t len, bool mayThrow) const
{
   return mChannels[ii]->GetSampleView(0u, start, len, mayThrow);
}

sampleCount WideClip::GetVisibleSampleCount() const
{
   return mChannels[0u]->GetVisibleSampleCount();
}

size_t WideClip::GetWidth() const
{
   return mChannels[1u] == nullptr ? 1u : 2u;
}

int WideClip::GetRate() const
{
   return mChannels[0u]->GetRate();
}

double WideClip::GetPlayStartTime() const
{
   return mChannels[0u]->GetPlayStartTime();
}

double WideClip::GetPlayEndTime() const
{
   return mChannels[0u]->GetPlayEndTime();
}

sampleCount WideClip::TimeToSamples(double time) const
{
   return mChannels[0u]->TimeToSamples(time);
}

double WideClip::GetStretchRatio() const
{
   return mChannels[0u]->GetStretchRatio();
}

int WideClip::GetCentShift() const
{
   return mChannels[0u]->GetCentShift();
}

PitchAndSpeedPreset WideClip::GetPitchAndSpeedPreset() const
{
   return mChannels[0u]->GetPitchAndSpeedPreset();
}

Observer::Subscription
WideClip::SubscribeToCentShiftChange(std::function<void(int)> cb)
{
   // On purpose set the publisher on the left channel only. This is not a clip
   // property that is saved to disk, and else we'll get two callbacks for the
   // same event.
   return mChannels[0u]->SubscribeToCentShiftChange(std::move(cb));
}

Observer::Subscription
WideClip::SubscribeToPitchAndSpeedPresetChange(std::function<void(PitchAndSpeedPreset)> cb)
{
   // On purpose set the publisher on the left channel only. This is not a clip
   // property that is saved to disk, and else we'll get two callbacks for the
   // same event.
   return mChannels[0u]->SubscribeToPitchAndSpeedPresetChange(std::move(cb));
}
