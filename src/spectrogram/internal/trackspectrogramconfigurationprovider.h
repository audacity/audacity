/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfigurationprovider.h"
#include "iglobalspectrogramconfiguration.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class TrackSpectrogramConfigurationProvider final : public ITrackSpectrogramConfigurationProvider
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

public:
    ~TrackSpectrogramConfigurationProvider() override = default;

    void init();

    ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const override;
    void copyConfiguration(const ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const override;
};
}
