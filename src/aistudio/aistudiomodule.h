/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::aistudio {
class AIStudioController;
class AIStudioUiActions;

class AIStudioModule final : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerUiTypes() override;
    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class AIStudioContext final : public muse::modularity::IContextSetup
{
public:
    AIStudioContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<AIStudioController> m_controller;
    std::shared_ptr<AIStudioUiActions> m_uiActions;
};
}
