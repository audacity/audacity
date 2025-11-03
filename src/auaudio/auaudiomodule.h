/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::auaudio {
class AudioThreadSecurer;
class AuAudioModule : public muse::modularity::IModuleSetup
{
public:
    AuAudioModule();

private:
    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    const std::shared_ptr<AudioThreadSecurer> m_audioThreadSecurer;
};
}
