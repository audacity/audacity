/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "trackedittypes.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
struct TrackData
{
    std::shared_ptr<au3::Au3Track> track;
    au::trackedit::ClipKey clipKey;

    inline bool isValid() const { return clipKey.isValid() && track != nullptr; }
};

class ITrackeditClipboard : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditClipboard)

public:
    virtual ~ITrackeditClipboard() = default;

    virtual std::vector<TrackData> trackData() const = 0;
    virtual TrackData trackData(size_t i) const = 0;
    virtual void clearTrackData() = 0;
    virtual bool trackDataEmpty() const = 0;
    virtual size_t trackDataSize() const = 0;
    virtual void addTrackData(const TrackData& trackData) = 0;
    virtual void eraseTrackData(std::vector<TrackData>::iterator begin, std::vector<TrackData>::iterator end) = 0;
};
}
