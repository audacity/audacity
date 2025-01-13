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

    virtual std::vector<TrackData> trackDataSource() const = 0;
    virtual std::vector<TrackData> trackDataCopy(int64_t newGroupId = -1) const = 0;
    virtual TrackData trackData(size_t i) const = 0;
    virtual void clearTrackData() = 0;
    virtual bool trackDataEmpty() const = 0;
    virtual size_t trackDataSize() const = 0;
    virtual void addTrackData(const TrackData& trackData) = 0;

    virtual void setMultiSelectionCopy(bool newValue) = 0;
    virtual bool isMultiSelectionCopy() const = 0;
};
}
