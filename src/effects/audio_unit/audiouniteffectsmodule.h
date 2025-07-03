/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class AudioUnitEffectsRepository;
class AudioUnitPluginsMetaReader;

class AudioUnitEffectsModule : public muse::modularity::IModuleSetup
{
public:
    AudioUnitEffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    const std::shared_ptr<AudioUnitPluginsMetaReader> m_metaReader;
    std::shared_ptr<AudioUnitEffectsRepository> m_effectsRepository;
};
}
