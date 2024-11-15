/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <itrackeditclipboard.h>

#include "Track.h"

#include "trackedittypes.h"

namespace au::trackedit {

class Au3TrackeditClipboard : public ITrackeditClipboard
{
public:
    std::vector<TrackData> trackData() const override;
    TrackData trackData(size_t i) const override;
    void clearTrackData() override;
    bool trackDataEmpty() const override;
    size_t trackDataSize() const override;
    void addTrackData(const TrackData& trackData) override;

private:
    std::vector<TrackData> m_tracksData;
};

}
