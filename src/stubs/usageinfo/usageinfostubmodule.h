/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::usageinfo {
class UsageInfoModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
};
}
