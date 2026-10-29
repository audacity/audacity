/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "framework/global/modularity/imodulesetup.h"

namespace au::macromanager {
class MacrosRepository;
class RunOnFilesRequests;
class MacroRunner;
class RunOnFilesStarter;

class MacroManagerModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;

    void registerExports() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<MacrosRepository> m_repository;
    std::shared_ptr<RunOnFilesRequests> m_runOnFilesRequests;
};

class MacroManagerContext : public muse::modularity::IContextSetup
{
public:
    MacroManagerContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onAllInited(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<MacroRunner> m_runner;
    std::shared_ptr<RunOnFilesStarter> m_runOnFilesStarter;
};
}
