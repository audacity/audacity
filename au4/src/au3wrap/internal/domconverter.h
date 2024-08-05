#pragma once

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "trackedit/dom/clip.h"

namespace au::au3 {
class DomConverter
{
public:

    // Tracks
    static trackedit::TrackId trackId(const TrackId& au3trackId);

    static trackedit::Clip clip(const WaveTrack* waveTrack, const WaveClip* au3clip, int index);
};
}
