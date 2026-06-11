/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::usageinfo {
class UsageInfoService;

class UsageInfoModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<UsageInfoService> m_usageInfoService;
};
}
