/*
 * Audacity: A Digital Audio Editor
 */

#include "trackspectrogramsettingsupdater.h"
#include "au3/au3trackspectrogramconfiguration.h"

#include "framework/global/log.h"

namespace au::trackedit {
void TrackSpectrogramSettingsUpdater::init()
{
    globalSpectrogramConfiguration()->someSettingChanged().onNotify(this, [this]{
        maybeApplyGlobalSettingsToTrack();
    });

    appShellConfiguration()->settingsApplied().onNotify(this, [this]{
        forEachTrack([](ITrackeditProject& trackeditProject, const Track& track, Au3TrackSpectrogramConfiguration& config){
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
    forEachTrack([this](ITrackeditProject&, const Track&, Au3TrackSpectrogramConfiguration& config){
        if (config.useGlobalSettings()) {
            config.setAllSettings(globalSpectrogramConfiguration()->allSettings());
        }
    });
}

void TrackSpectrogramSettingsUpdater::forEachTrack(std::function<void(ITrackeditProject&, const trackedit::Track&,
                                                                      Au3TrackSpectrogramConfiguration&)> func) const
{
    const auto trackeditProject = globalContext()->currentTrackeditProject();
    if (!trackeditProject) {
        return;
    }
    for (const Track& track : trackeditProject->trackList()) {
        const auto trackConfig = Au3TrackSpectrogramConfiguration::create(track.id, *globalContext());
        IF_ASSERT_FAILED(trackConfig) {
            continue;
        }
        func(*trackeditProject, track, *trackConfig);
    }
}
}
