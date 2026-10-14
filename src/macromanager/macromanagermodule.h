/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::macromanager {
class MacrosRepository;
class MacroRunner;

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
};

class MacroManagerContext : public muse::modularity::IContextSetup
{
public:
    MacroManagerContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;

private:
    std::shared_ptr<MacroRunner> m_runner;
};
}
