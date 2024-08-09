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
    audio::secs_t singleStep(audio::secs_t time, SnapType type, bool triplets, Direction direction) const;

private:
    double snapTypeMultiplier(SnapType type, bool triplets) const;

    double barMultiplier() const;
    double beatsMultiplier(SnapType type, bool triplets) const;

    double determineStep(double multiplier, Direction direction) const;
};
}
