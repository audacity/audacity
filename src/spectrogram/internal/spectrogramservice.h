/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramservice.h"
#include "iglobalspectrogramconfiguration.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class SpectrogramService final : public ISpectrogramService, public muse::Injectable
{
    muse::GlobalInject<IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

    muse::Inject<context::IGlobalContext> globalContext { this };

public:
    SpectrogramService(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}
    ~SpectrogramService() override = default;

    void init();

    ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const override;
    muse::async::Channel<int> trackSpectrogramConfigurationChanged() const override;
    void notifyAboutTrackSpectrogramConfigurationChanged(int trackId) override;

    void copyConfiguration(ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const override;
    double frequencyHardMaximum(int trackId) const override;
    double yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const override;
    double frequencyToY(int trackId, double frequency, double spectrogramHeight) const override;

private:
    double trackSampleRate(int trackId) const;

    muse::async::Channel<int> m_trackSpectrogramConfigurationChanged;
};
}
