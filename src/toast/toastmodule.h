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

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<ToastProvider> m_toastProvider;
    std::shared_ptr<ToastService> m_toastService;
};

class ToastContext : public muse::modularity::IContextSetup
{
public:
    ToastContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void onDeinit() override;
};
}
