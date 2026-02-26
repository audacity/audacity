/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKMODULE_H
#define AU_PLAYBACK_PLAYBACKMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::playback {
class PlaybackConfiguration;
class PlaybackController;
class PlaybackUiActions;
class Au3Playback;

class PlaybackModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<PlaybackConfiguration> m_configuration;
};

class PlaybackContext : public muse::modularity::IContextSetup
{
public:
    PlaybackContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<PlaybackController> m_controller;
    std::shared_ptr<PlaybackUiActions> m_uiActions;
    std::shared_ptr<Au3Playback> m_playback;
};
}

#endif // AU_PLAYBACK_PLAYBACKMODULE_H
