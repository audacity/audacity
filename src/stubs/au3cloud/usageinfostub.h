/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iusageinfo.h"
#include "framework/global/modularity/ioc.h"

namespace au::au3cloud {
class UsageInfoStub : public IUsageInfo
{
public:
    void setSendAnonymousUsageInfo(bool allow) override;
    bool getSendAnonymousUsageInfo() const override;
};
}
