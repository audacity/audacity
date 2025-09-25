/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "async/notification.h"

namespace au::record {
class IRecordMeterController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecordMeterController)

public:
    virtual ~IRecordMeterController() = default;

    [[nodiscard]] virtual bool isRecordMeterVisible() const = 0;
    virtual void setRecordMeterVisible(bool visible) = 0;
    [[nodiscard]] virtual muse::async::Notification isRecordMeterVisibleChanged() const = 0;
};
}
