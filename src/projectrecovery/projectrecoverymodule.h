/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::projectrecovery {
class ProjectRecoveryCommandsController;
class ProjectRecoveryCommandsRegister;
class ProjectRecoveryCommandsState;
class ProjectRecoveryService;

class ProjectRecoveryModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<ProjectRecoveryCommandsRegister> m_commandsRegister;
    std::shared_ptr<ProjectRecoveryService> m_service;
};

class ProjectRecoveryContext : public muse::modularity::IContextSetup
{
public:
    explicit ProjectRecoveryContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<ProjectRecoveryCommandsController> m_commandsController;
    std::shared_ptr<ProjectRecoveryCommandsState> m_commandsState;
};
}
