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
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<spectrogram::ISpectrogramService> spectrogramService;

public:
    void init();
    void forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&, spectrogram::ITrackSpectrogramConfiguration&)>) const;
    void maybeApplyGlobalSettingsToTrack();
};
}
