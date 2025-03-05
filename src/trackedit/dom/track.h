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

struct Track {
    TrackId id;
    muse::String title;
    TrackType type = TrackType::Undefined;
    muse::draw::Color color;

    inline bool operator==(const Track& k) const
    {
        return id == k.id &&
               title == k.title &&
               type == k.type &&
               color == k.color;
    }

    inline bool operator!=(const Track& k) const { return !this->operator==(k); }
};

using TrackList = std::vector<Track>;
}
