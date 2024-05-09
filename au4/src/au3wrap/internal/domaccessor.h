#pragma once

#include "libraries/lib-wave-track/WaveTrack.h"

namespace au::au3 {
class DomAccessor
{
public:

    static WaveTrack* findWaveTrack(AudacityProject& prj, const TrackId& au3trackId);
};
}
