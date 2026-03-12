/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
struct RulerGuide {
    int trackId = -1;
    double frequency = -1;

    constexpr bool operator==(const RulerGuide& other) const
    {
        return trackId == other.trackId && frequency == other.frequency;
    }

    constexpr bool operator!=(const RulerGuide& other) const
    {
        return !(*this == other);
    }
};
}
