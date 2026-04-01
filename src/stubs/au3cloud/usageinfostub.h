/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3cloud/iusageinfo.h"
#include "framework/global/modularity/ioc.h"

namespace au::au3cloud {
class UsageInfoStub : public IUsageInfo, public muse::Injectable
{
public:
    UsageInfoStub(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void setSendAnonymousUsageInfo(bool allow) override;
    bool getSendAnonymousUsageInfo() const override;
};
}
