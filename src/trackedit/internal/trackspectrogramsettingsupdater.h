/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"
#include "spectrogram/ispectrogramconfiguration.h"
#include "appshell/iappshellconfiguration.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <functional>

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration;
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<spectrogram::ISpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<au::appshell::IAppShellConfiguration> appShellConfiguration;

public:
    void init();
    void forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&, Au3TrackSpectrogramConfiguration&)>) const;
    void maybeApplyGlobalSettingsToTrack();
};
}
