/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PLAYBACK_PLAYBACKMODULE_H
#define AU_PLAYBACK_PLAYBACKMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::playback {
class PlaybackController;
class PlaybackUiActions;
class PlaybackModule : public muse::modularity::IModuleSetup
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
    std::shared_ptr<PlaybackController> m_controller;
    std::shared_ptr<PlaybackUiActions> m_uiActions;
};
}

#endif // AU_PLAYBACK_PLAYBACKMODULE_H
