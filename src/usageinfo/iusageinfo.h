/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::usageinfo {
class IUsageInfo : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IUsageInfo)
public:
    virtual ~IUsageInfo() = default;

    virtual void setSendAnonymousUsageInfo(bool allow) = 0;
    virtual bool getSendAnonymousUsageInfo() const = 0;

    virtual std::string instanceId() const = 0;

    virtual void setUserId(const std::string& userId) = 0;

    virtual muse::async::Notification usageInfoChanged() const = 0;
};
}
