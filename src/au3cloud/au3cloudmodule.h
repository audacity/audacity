/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::au3cloud {
class Au3CloudService;
class Au3AudioComService;
class Au3AudioComController;

class Au3CloudModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
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
    std::shared_ptr<Au3CloudService> m_cloudService;
    std::shared_ptr<Au3AudioComService> m_audioComService;
    std::shared_ptr<Au3AudioComController> m_audioComController;
};
}
