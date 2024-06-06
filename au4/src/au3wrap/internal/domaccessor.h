#pragma once

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"

namespace au::au3 {
class DomAccessor
{
public:

    static WaveTrack* findWaveTrack(AudacityProject& prj, const TrackId& au3trackId);
    static std::shared_ptr<WaveClip> findWaveClip(WaveTrack* track, size_t index);
    static std::shared_ptr<WaveClip> findWaveClip(AudacityProject& prj, const TrackId& au3trackId, size_t index);
};
}
