/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

namespace au::au3cloud {
class IUsageInfo : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IUsageInfo)
public:
    virtual ~IUsageInfo() = default;

    virtual void setSendAnonymousUsageInfo(bool allow) = 0;
    virtual bool getSendAnonymousUsageInfo() const = 0;
};
}
