/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"
#include "spectrogram/ispectrogramconfiguration.h"
#include "appshell/iappshellconfiguration.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

namespace au::trackedit {
class TrackSpectrogramSettingsUpdater : public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<spectrogram::ISpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<au::appshell::IAppShellConfiguration> appShellConfiguration;

public:
    void init();
    void onSomeSettingChanged();
};
}
