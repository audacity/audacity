#ifndef AU_PROСESSING_TRACK_H
#define AU_PROСESSING_TRACK_H

#include <vector>

#include "clip.h"

namespace au::processing {
class Track
{
public:
    Track() = default;

    const Clips& clips() const;
    void setClips(const Clips& c);

private:

    Clips m_clips;
};

using TrackList = std::vector<Track>;
}

#endif // AU_PROСESSING_TRACK_H
