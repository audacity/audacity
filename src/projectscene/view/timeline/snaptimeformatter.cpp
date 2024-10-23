/*
* Audacity: A Digital Audio Editor
*/
#include "snaptimeformatter.h"

#include "global/containers.h"

#include "playback/iaudiooutput.h"

using namespace au::projectscene;

static bool isBarSnap(SnapType type)
{
    return type == SnapType::Bar;
}

static bool isBeatsSnap(SnapType type)
{
    static const std::vector<SnapType> types = {
        SnapType::Half,
        SnapType::Quarter,
        SnapType::Eighth,
        SnapType::Sixteenth,
        SnapType::ThirtySecond,
        SnapType::SixtyFourth,
        SnapType::HundredTwentyEighth
    };

    return muse::contains(types, type);
}

static int timeDivisor(SnapType type)
{
    static const std::map<SnapType, int> divisors = {
        { SnapType::Half, 2 },
        { SnapType::Quarter, 4 },
        { SnapType::Eighth, 8 },
        { SnapType::Sixteenth, 16 },
        { SnapType::ThirtySecond, 32 },
        { SnapType::SixtyFourth, 64 },
        { SnapType::HundredTwentyEighth, 128 }
    };

    return muse::value(divisors, type, 1);
}

static bool isTimeSnap(SnapType type)
{
    static const std::vector<SnapType> types = {
        SnapType::Seconds,
        SnapType::Deciseconds,
        SnapType::Centiseconds,
        SnapType::Milliseconds,
    };

    return muse::contains(types, type);
}

static double timeMultiplier(SnapType type)
{
    static const std::map<SnapType, int> multipliers = {
        { SnapType::Seconds, 1.0 },
        { SnapType::Deciseconds, 10.0 },
        { SnapType::Centiseconds, 100.0 },
        { SnapType::Milliseconds, 1000.0 }
    };

    return muse::value(multipliers, type, 1.0);
}

static bool isSamplesSnap(SnapType type)
{
    return type == SnapType::Samples;
}

static bool isFrameSnap(SnapType type)
{
    static const std::vector<SnapType> types = {
        SnapType::FilmFrames,
        SnapType::NTSCFrames,
        SnapType::NTSCFramesDrop,
        SnapType::PALFrames,
        SnapType::CDDAFrames
    };

    return muse::contains(types, type);
}

static double frameMultiplier(SnapType type)
{
    static const std::map<SnapType, int> rates = {
        { SnapType::FilmFrames, 24.0 },
        { SnapType::NTSCFrames, 30.0 / 1.001 },
        { SnapType::NTSCFramesDrop, 30.0 / 1.001 },
        { SnapType::PALFrames, 25.0 },
        { SnapType::CDDAFrames, 75.0 }
    };

    return muse::value(rates, type, 1);
}

muse::secs_t SnapTimeFormatter::snapTime(muse::secs_t time, const Snap& snap, trackedit::TimeSignature timeSig) const
{
    double multiplier = snapTypeMultiplier(snap.type, snap.isSnapTriplets, timeSig);

    if (!muse::RealIsNull(multiplier)) {
        return std::round(time * multiplier) / multiplier;
    }
    return 0.0;
}

muse::secs_t SnapTimeFormatter::singleStep(muse::secs_t time, const Snap& snap, Direction direction,
                                           trackedit::TimeSignature timeSig) const
{
    double multiplier = snapTypeMultiplier(snap.type, snap.isSnapTriplets, timeSig);

    if (!muse::RealIsNull(multiplier)) {
        return std::round(time * multiplier) / multiplier + determineStep(multiplier, direction);
    }
    return 0.0;
}

double SnapTimeFormatter::snapTypeMultiplier(SnapType type, bool triplets, trackedit::TimeSignature timeSig) const
{
    double multiplier = 0.0;
    if (isBarSnap(type)) {
        multiplier = barMultiplier(timeSig);
    } else if (isBeatsSnap(type)) {
        multiplier = beatsMultiplier(type, triplets, timeSig);
    } else if (isTimeSnap(type)) {
        multiplier = timeMultiplier(type);
    } else if (isSamplesSnap(type)) {
        multiplier = playback()->audioOutput()->sampleRate();
    } else if (isFrameSnap(type)) {
        multiplier = frameMultiplier(type);
    }

    return multiplier;
}

double SnapTimeFormatter::barMultiplier(trackedit::TimeSignature timeSig) const
{
    double quarterDuration = 60.0 / timeSig.tempo;
    double beatDuration = quarterDuration * 4.0 / timeSig.lower;
    double barDuration = beatDuration * timeSig.upper;
    double multiplier = 1 / barDuration;

    return multiplier;
}

double SnapTimeFormatter::beatsMultiplier(SnapType type, bool triplets, trackedit::TimeSignature timeSig) const
{
    double quarterDuration = 60.0 / timeSig.tempo;

    int divisor = timeDivisor(type);
    if (triplets) {
        divisor = 3 * (divisor / 2);
    }

    double fracDuration = quarterDuration * 4.0 / divisor;
    double multiplier = 1.0 / fracDuration;

    return multiplier;
}

double SnapTimeFormatter::determineStep(double multiplier, Direction direction) const
{
    if (direction == Direction::Left) {
        return -1 / multiplier;
    } else {
        return 1 / multiplier;
    }
}
