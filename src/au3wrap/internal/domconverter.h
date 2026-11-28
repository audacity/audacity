#pragma once

#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"
#include "trackedit/dom/label.h"

#include "../au3types.h"

#include "au3-time-and-pitch/StretchMethod.h"

namespace au::au3 {
class DomConverter
{
public:

    static trackedit::Clip clip(const Au3WaveTrack* waveTrack, const Au3WaveClip* au3clip);

    static trackedit::ClipStretchMethod stretchMethod(StretchMethod method);
    static StretchMethod stretchMethod(trackedit::ClipStretchMethod method);

    static trackedit::Track track(const Au3Track* track);

    static trackedit::Track labelTrack(const Au3LabelTrack* labelTrack);

    static trackedit::Label label(const Au3LabelTrack* labelTrack, const Au3Label* au3label);
};
}
