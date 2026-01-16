/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/ispectrogramservice.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <functional>

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration;
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<spectrogram::ISpectrogramService> spectrogramService { this };

public:
    TrackSpectrogramSettingsUpdater(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();
    void forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&, spectrogram::ITrackSpectrogramConfiguration&)>) const;
    void maybeApplyGlobalSettingsToTrack();
};
}
