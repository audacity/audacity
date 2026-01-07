/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramservice.h"
#include "iglobalspectrogramconfiguration.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class SpectrogramService final : public ISpectrogramService
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

public:
    ~SpectrogramService() override = default;

    void init();

    ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const override;
    void copyConfiguration(const ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const override;
    double yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const override;
};
}
