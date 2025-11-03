/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::audio {
class AudioThreadSecurer;
class AudioModule : public muse::modularity::IModuleSetup
{
public:
    AudioModule();

private:
    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    const std::shared_ptr<AudioThreadSecurer> m_audioThreadSecurer;
};
}
