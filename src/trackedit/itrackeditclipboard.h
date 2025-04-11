/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "trackedittypes.h"
#include "itrackdata.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class ITrackeditClipboard : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditClipboard)

public:
    virtual ~ITrackeditClipboard() = default;

    virtual std::vector<ITrackDataPtr> trackDataCopy() const = 0;
    virtual void clearTrackData() = 0;
    virtual bool trackDataEmpty() const = 0;
    virtual size_t trackDataSize() const = 0;
    virtual void addTrackData(ITrackDataPtr) = 0;

    virtual void setMultiSelectionCopy(bool newValue) = 0;
    virtual bool isMultiSelectionCopy() const = 0;
};
}
