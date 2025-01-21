/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::audio {
class AudioEngine;
class AudioThreadSecurer;
class AudioModule : public muse::modularity::IModuleSetup
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

    std::shared_ptr<AudioEngine> m_audioEngine;
    std::shared_ptr<AudioThreadSecurer> m_audioThreadSecurer;
};
}
