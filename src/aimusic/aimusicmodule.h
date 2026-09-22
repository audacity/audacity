/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::aimusic {
class AiMusicController;
class AiMusicUiActions;

class AiMusicModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class AiMusicContext : public muse::modularity::IContextSetup
{
public:
    AiMusicContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<AiMusicController> m_controller;
    std::shared_ptr<AiMusicUiActions> m_uiActions;
};
}
