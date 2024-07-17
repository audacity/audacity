#pragma once

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "processing/dom/clip.h"

namespace au::au3 {
class DomConverter
{
public:

    // Tracks
    static processing::TrackId trackId(const TrackId& au3trackId);

    static processing::Clip clip(const WaveTrack* waveTrack, const WaveClip* au3clip);
};
}
