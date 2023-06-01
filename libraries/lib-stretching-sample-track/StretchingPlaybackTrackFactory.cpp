#include "StretchingPlaybackTrackFactory.h"

#include "StretchingSampleTrack.h"
#include "WaveTrack.h"

std::function<ConstSampleTrackHolder(SampleTrackHolder)>
StretchingPlaybackTrackFactory::GetStretchingSampleTrackFactory(double t0)
{
   return [t0](SampleTrackHolder sampleTrack) -> ConstSampleTrackHolder {
      if (track_cast<WaveTrack*>(sampleTrack.get()))
      {
         return std::make_shared<StretchingSampleTrack>(
            std::static_pointer_cast<WaveTrack>(sampleTrack), t0);
      }
      else
      {
         return sampleTrack;
      }
   };
}
