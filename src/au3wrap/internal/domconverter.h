#pragma once

#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"
#include "trackedit/dom/label.h"

#include "../au3types.h"

namespace au::au3 {
class DomConverter
{
public:

    static trackedit::Clip clip(const Au3WaveTrack* waveTrack, const Au3WaveClip* au3clip);

    static trackedit::Track track(const Au3Track* waveTrack);

    static trackedit::Track labelTrack(const Au3LabelTrack* labelTrack);

    static trackedit::Label label(const Au3LabelTrack* labelTrack, size_t index, const Au3Label& au3label);
};
}
