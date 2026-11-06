/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <optional>

namespace au::au4juce {
//! What an EndOfTrackNode needs besides its track
class IEndOfTrackContext
{
public:
    virtual ~IEndOfTrackContext() = default;

    //! Where the play range ends, in the timeline, in samples; none while looping. Any thread: it changes when looping is
    //! turned off during playback.
    virtual std::optional<int64_t> playEnd() const = 0;
};
}
