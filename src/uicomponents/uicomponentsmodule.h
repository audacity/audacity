/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::uicomponents {
class UiComponentsModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;
};
}
