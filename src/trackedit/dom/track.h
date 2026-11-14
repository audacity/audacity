/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "../trackedittypes.h"
#include "global/types/string.h"
#include "global/types/translatablestring.h"

#include "clip.h" // IWYU pragma: export
#include "label.h" // IWYU pragma: export

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

enum class TrackRulerType : int {
    Linear = 0,
    DbLinear,
    DbLog
};

enum class TrackViewType : int {
    Undefined = -1,
    Waveform,
    Spectrogram,
    WaveformAndSpectrogram,
};

struct TrackFormatInfo {
    TrackFormat format;
    muse::TranslatableString description;
};

inline const std::array<TrackFormatInfo, 3>& availableTrackFormats()
{
    static const std::array<TrackFormatInfo, 3> AVAILABLE_TRACK_FORMATS = { {
        //: The format of the audio samples on a track
        { au::trackedit::TrackFormat::Int16, muse::TranslatableString("trackcontextmenu", "16-bit PCM") },
        //: The format of the audio samples on a track
        { au::trackedit::TrackFormat::Int24, muse::TranslatableString("trackcontextmenu", "24-bit PCM") },
        //: The format of the audio samples on a track
        { au::trackedit::TrackFormat::Float32, muse::TranslatableString("trackcontextmenu", "32-bit Float") }
    } };
    return AVAILABLE_TRACK_FORMATS;
}

struct Track {
    TrackId id;
    muse::String title;
    TrackType type = TrackType::Undefined;
    ClipColorIndex colorIndex = 1;
    TrackFormat format = TrackFormat::Undefined;
    uint64_t rate = 0;
    bool solo = false;
    bool mute = false;
};

using TrackList = std::vector<Track>;
}
