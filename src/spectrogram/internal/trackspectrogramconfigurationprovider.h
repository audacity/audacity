/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfigurationprovider.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class TrackSpectrogramConfigurationProvider final : public ITrackSpectrogramConfigurationProvider
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    ~TrackSpectrogramConfigurationProvider() override = default;

    ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const override;
};
}
