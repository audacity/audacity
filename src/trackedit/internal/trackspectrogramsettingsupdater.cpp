/*
 * Audacity: A Digital Audio Editor
 */

#include "trackspectrogramsettingsupdater.h"

#include "framework/global/log.h"

namespace au::trackedit {
void TrackSpectrogramSettingsUpdater::init()
{
    globalSpectrogramConfiguration()->someSettingChanged().onNotify(this, [this]{
        maybeApplyGlobalSettingsToTrack();
    });

    appShellConfiguration()->settingsApplied().onNotify(this, [this]{
        forEachTrack([](ITrackeditProject& trackeditProject, const Track& track, spectrogram::ITrackSpectrogramConfiguration& config){
            if (config.useGlobalSettings()) {
                trackeditProject.notifyAboutTrackChanged(track);
            }
        });
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]{
        maybeApplyGlobalSettingsToTrack();
    });

    maybeApplyGlobalSettingsToTrack();
}

void TrackSpectrogramSettingsUpdater::maybeApplyGlobalSettingsToTrack()
{
    forEachTrack([this](ITrackeditProject&, const Track&, spectrogram::ITrackSpectrogramConfiguration& config){
        if (config.useGlobalSettings()) {
            config.setAllSettings(globalSpectrogramConfiguration()->allSettings());
        }
    });
}

void TrackSpectrogramSettingsUpdater::forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&,
                                                                      spectrogram::ITrackSpectrogramConfiguration&)> func) const
{
    const auto trackeditProject = globalContext()->currentTrackeditProject();
    if (!trackeditProject) {
        return;
    }
    for (const Track& track : trackeditProject->trackList()) {
        const auto trackConfig = trackSpectrogramConfigurationProvider()->trackSpectrogramConfiguration(track.id);
        IF_ASSERT_FAILED(trackConfig) {
            continue;
        }
        func(*trackeditProject, track, *trackConfig);
    }
}
}
