/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imodulesetup.h"

namespace au::spectrogram {
class SpectrogramConfiguration;

class SpectrogramModule : public muse::modularity::IModuleSetup
{
public:
    SpectrogramModule();
    ~SpectrogramModule() override = default;

public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode&) override;

private:
    const std::shared_ptr<SpectrogramConfiguration> m_configuration;
};
}
