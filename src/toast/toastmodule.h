/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <string>

#include "modularity/imodulesetup.h"

namespace au::toast {
class Toast;
class ToastProvider;
class ToastModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Toast> m_toast;
    std::shared_ptr<ToastProvider> m_toastProvider;
};
}
