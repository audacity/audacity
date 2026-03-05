/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "itrackdata.h"

namespace au::trackedit {
class IClipboardData : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IClipboardData)

public:
    virtual ~IClipboardData() = default;

    virtual std::vector<ITrackDataPtr> trackData() const = 0;
    virtual void addTrackData(ITrackDataPtr data) = 0;
    virtual void clearTrackData() = 0;
    virtual bool trackDataEmpty() const = 0;
    virtual size_t trackDataSize() const = 0;

    virtual void setMultiSelectionCopy(bool val) = 0;
    virtual bool isMultiSelectionCopy() const = 0;
};
}
