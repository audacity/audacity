/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"

#include "types/projectscenetypes.h"
#include "playback/audiotypes.h"

namespace au::projectscene {
class SnapTimeFormatter
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<playback::IPlayback> playback;

public:
    audio::secs_t snapTime(audio::secs_t time, SnapType type, bool triplets) const;

private:
    audio::secs_t snapTimeBar(audio::secs_t time) const;
    audio::secs_t snapTimeBeats(audio::secs_t time, SnapType type, bool triplets) const;
    audio::secs_t snapTimeTime(audio::secs_t time, SnapType type) const;
    audio::secs_t snapTimeSamples(audio::secs_t time) const;
    audio::secs_t snapTimeFrames(audio::secs_t time, SnapType type) const;
};
}
