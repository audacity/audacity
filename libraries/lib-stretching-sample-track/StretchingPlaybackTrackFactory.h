#pragma once

#include "SampleTrack.h"

using SampleTrackHolder = std::shared_ptr<SampleTrack>;
using ConstSampleTrackHolder = std::shared_ptr<const SampleTrack>;

class STRETCHING_SAMPLE_TRACK_API StretchingPlaybackTrackFactory final
{
public:
   static std::function<ConstSampleTrackHolder(SampleTrackHolder)>
   GetStretchingSampleTrackFactory(double t0);
};
