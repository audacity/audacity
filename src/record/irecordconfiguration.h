/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "draw/types/color.h"

namespace au::record {
class IRecordConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecordConfiguration)

public:
    virtual ~IRecordConfiguration() = default;

    virtual muse::draw::Color recordColor() const = 0;

    virtual bool isMicMeteringOn() const = 0;
    virtual void setIsMicMeteringOn(bool enable) = 0;
    virtual muse::async::Notification isMicMeteringOnChanged() const = 0;

    virtual bool isInputMonitoringOn() const = 0;
    virtual void setIsInputMonitoringOn(bool enable) = 0;
    virtual muse::async::Notification isInputMonitoringOnChanged() const = 0;
};
}
