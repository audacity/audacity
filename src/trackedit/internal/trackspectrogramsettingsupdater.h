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
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<spectrogram::ISpectrogramService> spectrogramService { this };

public:
    TrackSpectrogramSettingsUpdater(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&, spectrogram::ITrackSpectrogramConfiguration&)>) const;
    void maybeApplyGlobalSettingsToTrack();
};
}
