/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

#include "internal/au3cloudservice.h"

namespace au::au3cloud {
class Au3CloudModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void registerUiTypes() override;

private:
    std::shared_ptr<Au3CloudService> m_cloudService;
};
}
