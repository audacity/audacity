/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/itrackspectrogramconfigurationprovider.h"
#include "appshell/iappshellconfiguration.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <functional>

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration;
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<au::appshell::IAppShellConfiguration> appShellConfiguration;
    muse::Inject<spectrogram::ITrackSpectrogramConfigurationProvider> trackSpectrogramConfigurationProvider;

public:
    void init();
    void forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&, spectrogram::ITrackSpectrogramConfiguration&)>) const;
    void maybeApplyGlobalSettingsToTrack();
};
}
