/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "../trackedittypes.h"
#include "global/types/string.h"
#include "draw/types/color.h"

#include "clip.h" // IWYU pragma: export

namespace au::trackedit {
enum class TrackType {
    Undefined,
    Mono,
    Stereo,
    Label
};

enum class TrackFormat : int {
    Undefined = 0,
    Int16,
    Int24,
    Float32
};

struct TrackFormatInfo {
    TrackFormat format;
    const char* description;
};

inline const std::array<TrackFormatInfo, 3>& availableTrackFormats()
{
    static constexpr std::array<TrackFormatInfo, 3> AVAILABLE_TRACK_FORMATS = { {
        { au::trackedit::TrackFormat::Int16, "16-bit PCM" },
        { au::trackedit::TrackFormat::Int24, "24-bit PCM" },
        { au::trackedit::TrackFormat::Float32, "32-bit Float" }
    } };
    return AVAILABLE_TRACK_FORMATS;
}

inline const std::array<int, 12>& availableTrackSampleRates()
{
    static constexpr std::array<int, 12> AVAILABLE_SAMPLE_RATES = { {
        8000, 11025, 16000, 22050, 44100, 48000,
        88200, 96000, 176400, 192000, 352800, 384000
    } };
    return AVAILABLE_SAMPLE_RATES;
}

struct Track {
    TrackId id;
    muse::String title;
    TrackType type = TrackType::Undefined;
    muse::draw::Color color;
    TrackFormat format = TrackFormat::Undefined;
    int rate;
};

using TrackList = std::vector<Track>;
}
