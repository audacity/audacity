/*
* Audacity: A Digital Audio Editor
*/
#include "snaptimeformatter.h"

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

au::audio::secs_t SnapTimeFormatter::snapTime(audio::secs_t time, SnapType type, bool triplets) const
{
    if (isBarSnap(type)) {
        return snapTimeBar(time);
    } else if (isBeatsSnap(type)) {
        return snapTimeBeats(time, type, triplets);
    } else if (isTimeSnap(type)) {
        return snapTimeTime(time, type);
    } else if (isSamplesSnap(type)) {
        return snapTimeSamples(time);
    } else if (isFrameSnap(type)) {
        return snapTimeFrames(time, type);
    }

    return 0.0;
}

au::audio::secs_t SnapTimeFormatter::snapTimeBar(audio::secs_t time) const
{
    auto project = globalContext()->currentProcessingProject();
    if (!project) {
        return 0.0;
    }

    processing::TimeSignature timeSig = project->timeSignature();

    double quarterDuration = 60.0 / timeSig.tempo;
    double beatDuration = quarterDuration * 4.0 / timeSig.lower;
    double barDuration = beatDuration * timeSig.upper;
    double multiplier = 1 / barDuration;

    return std::round(time * multiplier) / multiplier;
}

au::audio::secs_t SnapTimeFormatter::snapTimeBeats(audio::secs_t time, SnapType type, bool triplets) const
{
    auto project = globalContext()->currentProcessingProject();
    if (!project) {
        return 0.0;
    }

    processing::TimeSignature timeSig = project->timeSignature();

    double quarterDuration = 60.0 / timeSig.tempo;

    int divisor = timeDivisor(type);
    if (triplets) {
        divisor = 3 * (divisor / 2);
    }

    double fracDuration = quarterDuration * 4.0 / divisor;
    double multiplier = 1.0 / fracDuration;

    return std::round(time * multiplier) / multiplier;
}

au::audio::secs_t SnapTimeFormatter::snapTimeTime(audio::secs_t time, SnapType type) const
{
    double multiplier = timeMultiplier(type);

    return std::round(time * multiplier) / multiplier;
}

au::audio::secs_t SnapTimeFormatter::snapTimeSamples(audio::secs_t time) const
{
    double sampleRate = playback()->audioOutput()->sampleRate();

    return std::round(time * sampleRate) / sampleRate;
}

au::audio::secs_t SnapTimeFormatter::snapTimeFrames(audio::secs_t time, SnapType type) const
{
    double multiplier = frameMultiplier(type);

    return std::round(time * multiplier) / multiplier;
}
