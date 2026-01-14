/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <string>

#include "modularity/imodulesetup.h"

namespace au::toast {
class ToastService;
class ToastProvider;
class ToastModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;

private:
    std::shared_ptr<ToastService> m_toastService;
    std::shared_ptr<ToastProvider> m_toastProvider;
};
}
