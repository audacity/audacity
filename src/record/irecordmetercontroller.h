/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "async/notification.h"

namespace au::record {
class IRecordMeterController : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IRecordMeterController)

public:
    virtual ~IRecordMeterController() = default;

    [[nodiscard]] virtual bool isRecordMeterVisible() const = 0;
    virtual void setRecordMeterVisible(bool visible) = 0;
    [[nodiscard]] virtual muse::async::Notification isRecordMeterVisibleChanged() const = 0;

    [[nodiscard]] virtual bool isSoundActivationMeterVisible() const = 0;
    virtual void setSoundActivationMeterVisible(bool visible) = 0;
    [[nodiscard]] virtual muse::async::Notification isSoundActivationMeterVisibleChanged() const = 0;
};
}
