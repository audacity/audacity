/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "Track.h"
#include "modularity/imoduleinterface.h"
#include "trackedittypes.h"

namespace au::trackedit {

struct TrackData
{
    // Track type from Track.h
    std::shared_ptr<::Track> track;
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
    virtual void eraseFromBeginning(size_t i) = 0;
};
}
