/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::musehub {
class MuseHubCommandsController;
class MuseHubCommandsRegister;
class MuseHubCommandsState;

class MuseHubModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void resolveImports() override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<MuseHubCommandsRegister> m_commandsRegister;
};

class MuseHubContext : public muse::modularity::IContextSetup
{
public:
    explicit MuseHubContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<MuseHubCommandsController> m_commandsController;
    std::shared_ptr<MuseHubCommandsState> m_commandsState;
};
}
