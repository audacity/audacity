#ifndef AU_PROСESSING_TRACK_H
#define AU_PROСESSING_TRACK_H

#include <vector>

#include "global/types/id.h"
#include "global/types/string.h"

#include "clip.h"

namespace au::processing {
struct Track {
    mu::ID id;
    mu::String title;
};

using TrackList = std::vector<Track>;
}

#endif // AU_PROСESSING_TRACK_H
