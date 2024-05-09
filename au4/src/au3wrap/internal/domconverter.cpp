#include "domconverter.h"

#include "libraries/lib-track/Track.h"

#include "../wxtypes_convert.h"

using namespace au::au3;

au::processing::TrackId DomConverter::trackId(const TrackId& au3trackId)
{
    return *(reinterpret_cast<const long*>(&au3trackId));
}

au::processing::Clip DomConverter::clip(const WaveTrack* waveTrack, const WaveClip* au3clip, int index)
{
    au::processing::Clip clip;
    clip.key.trackId = trackId(waveTrack->GetId());
    clip.key.index = index;
    clip.au3WaveTrackPtr = reinterpret_cast<uintptr_t>(waveTrack);
    clip.au3WaveClipPtr = reinterpret_cast<uintptr_t>(au3clip);
    clip.title = wxToSting(au3clip->GetName());
    clip.startTime = au3clip->GetPlayStartTime();
    clip.endTime = au3clip->GetPlayEndTime();
    return clip;
}
