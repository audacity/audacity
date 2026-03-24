/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "draw/types/color.h"

namespace au::record {
class IRecordConfiguration : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IRecordConfiguration)

public:
    virtual ~IRecordConfiguration() = default;

    virtual bool isMicMeteringOn() const = 0;
    virtual void setIsMicMeteringOn(bool enable) = 0;
    virtual muse::async::Notification isMicMeteringOnChanged() const = 0;

    virtual bool isInputMonitoringOn() const = 0;
    virtual void setIsInputMonitoringOn(bool enable) = 0;
    virtual muse::async::Notification isInputMonitoringOnChanged() const = 0;

    virtual double preRollDuration() const = 0;
    virtual void setPreRollDuration(double seconds) = 0;
    virtual muse::async::Notification preRollDurationChanged() const = 0;

    virtual double crossfadeDuration() const = 0;
    virtual void setCrossfadeDuration(double milliseconds) = 0;
    virtual muse::async::Notification crossfadeDurationChanged() const = 0;
};
}
