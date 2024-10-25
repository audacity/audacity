#pragma once

#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"

#include "../au3types.h"

namespace au::au3 {
class DomConverter
{
public:

    static trackedit::Clip clip(const Au3WaveTrack* waveTrack, const Au3WaveClip* au3clip);

    static trackedit::Track track(const Au3Track* waveTrack);
};
}
