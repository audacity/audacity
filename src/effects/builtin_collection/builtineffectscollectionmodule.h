/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class BuiltinEffectsLoader;

class BuiltinEffectsCollectionModule : public muse::modularity::IModuleSetup
{
public:
    BuiltinEffectsCollectionModule();
    ~BuiltinEffectsCollectionModule() override;

    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onPreInit(const muse::IApplication::RunMode& mode) override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::unique_ptr<BuiltinEffectsLoader> m_builtinEffectsLoader;
};

class BuiltinEffectsCollectionContext : public muse::modularity::IContextSetup
{
public:
    BuiltinEffectsCollectionContext(const muse::modularity::ContextPtr& ctx);

    void registerExports() override;
    void resolveImports() override;
    void onDeinit() override;
};
}
