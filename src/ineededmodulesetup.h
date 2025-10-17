#pragma once

#include <modularity/imodulesetup.h>

namespace au {
class INeededModuleSetup : public muse::modularity::IModuleSetup
{
public:
    virtual ~INeededModuleSetup() = default;

    //! Takes ownership
    INeededModuleSetup(muse::modularity::IModuleSetup* module)
        : m_module{module} {}

    virtual bool isNeededForRunMode(const muse::IApplication::RunMode&) const { return false; }

    std::string moduleName() const override { return m_module->moduleName(); }
    void registerExports() override { m_module->registerExports(); }
    void resolveImports() override { m_module->resolveImports(); }
    void registerResources() override { m_module->registerResources(); }
    void registerUiTypes() override { m_module->registerUiTypes(); }
    void registerApi() override { m_module->registerApi(); }
    void onPreInit(const muse::IApplication::RunMode& mode) override { m_module->onPreInit(mode); }
    void onInit(const muse::IApplication::RunMode& mode) override { m_module->onInit(mode); }
    void onAllInited(const muse::IApplication::RunMode& mode) override { m_module->onAllInited(mode); }
    void onDelayedInit() override { m_module->onDelayedInit(); }
    void onDeinit() override { m_module->onDeinit(); }
    void onDestroy() override { m_module->onDestroy(); }
    void onStartApp() override { m_module->onStartApp(); }

private:
    std::unique_ptr<muse::modularity::IModuleSetup> m_module;
};
}
