/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "playback/iplayback.h"

#include "projectscene/iprojectviewstate.h"
#include "../../types/projectscenetypes.h"

namespace au::projectscene {
class SnapTimeFormatter
{
    muse::Inject<playback::IPlayback> playback;

public:
    muse::secs_t snapTime(muse::secs_t time, const Snap& snap, trackedit::TimeSignature timeSig) const;
    muse::secs_t singleStep(muse::secs_t time, const Snap& snap, Direction direction, trackedit::TimeSignature timeSig) const;

private:
    double snapTypeMultiplier(SnapType type, bool triplets, trackedit::TimeSignature timeSig) const;

    double barMultiplier(trackedit::TimeSignature timeSig) const;
    double beatsMultiplier(SnapType type, bool triplets, trackedit::TimeSignature timeSig) const;

    double determineStep(double multiplier, Direction direction) const;
};
}
