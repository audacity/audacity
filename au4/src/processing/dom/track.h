#ifndef AU_PROСESSING_TRACK_H
#define AU_PROСESSING_TRACK_H

#include <vector>

#include "../processingtypes.h"
#include "global/types/string.h"

#include "clip.h"

namespace au::processing {
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
};

using TrackList = std::vector<Track>;
}

#endif // AU_PROСESSING_TRACK_H
