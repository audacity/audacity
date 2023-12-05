/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WideClip.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "ClipInterface.h"

#include <array>

// A provisional class we can get rid of when Audacity actually uses wide clips.
// In the meantime, `WaveTrack::GetClipInterfaces()` must construct these
// querying clips of the other channel tracks if any. If that's not clear please
// take a look into the `GetClipInterface` implementation.
class WideClip : public ClipInterface
{
public:
   /*
    * @pre `left` is not null, and `right` is null or equal to `left` in
    * sample rate, play start time, play end time and stretch ratio.
    */
   WideClip(
      std::shared_ptr<ClipInterface> left,
      std::shared_ptr<ClipInterface> right);

   AudioSegmentSampleView GetSampleView(
      size_t ii, sampleCount start, size_t len, bool mayThrow) const override;

   sampleCount GetVisibleSampleCount() const override;

   size_t GetWidth() const override;

   int GetRate() const override;

   double GetPlayStartTime() const override;

   double GetPlayEndTime() const override;

   sampleCount TimeToSamples(double time) const override;

   double GetStretchRatio() const override;

private:
   const std::array<std::shared_ptr<ClipInterface>, 2> mChannels;
};
