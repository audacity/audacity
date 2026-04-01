/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::au3cloud {
class Au3AudioComServiceStub;
class AuthorizationStub;
class UsageInfoStub;

class Au3CloudModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class Au3CloudStubContext : public muse::modularity::IContextSetup
{
public:
    Au3CloudStubContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
};
}
