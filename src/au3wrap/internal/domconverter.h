#pragma once

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"

#include "../au3types.h"

namespace au::au3 {
class DomConverter
{
public:

    // Tracks
    static trackedit::TrackId trackId(const Au3TrackId& au3trackId);

    static trackedit::Clip clip(const Au3WaveTrack* waveTrack, const WaveClip* au3clip);

    static trackedit::Track track(const Au3Track* waveTrack);
};
}
