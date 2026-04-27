/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::au3cloud {
class Au3CloudConfiguration;
class Au3CloudService;
class Au3AudioComService;
class Au3CloudActionsController;
class CloudUiActions;

class Au3CloudModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<Au3CloudConfiguration> m_cloudConfiguration;
    std::shared_ptr<Au3CloudService> m_cloudService;
};

class Au3CloudContext : public muse::modularity::IContextSetup
{
public:
    Au3CloudContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Au3AudioComService> m_audioComService;
    std::shared_ptr<Au3CloudActionsController> m_actionsController;
    std::shared_ptr<CloudUiActions> m_uiActions;
};
}
