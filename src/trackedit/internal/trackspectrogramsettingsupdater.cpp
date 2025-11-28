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
        onSomeSettingChanged();
    });

    appShellConfiguration()->settingsApplied().onNotify(this, [this]{
        const auto trackeditProject = globalContext()->currentTrackeditProject();
        if (!trackeditProject) {
            // Likely changed from the prefs without a project open
            return;
        }
        for (const Track track : trackeditProject->trackList()) {
            const auto trackConfig = Au3TrackSpectrogramConfiguration::create(track.id, *globalContext());
            IF_ASSERT_FAILED(trackConfig) {
                continue;
            }
            if (trackConfig->useGlobalSettings()) {
                trackeditProject->notifyAboutTrackChanged(track);
            }
        }
    });
}

void TrackSpectrogramSettingsUpdater::onSomeSettingChanged()
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        // Likely changed from the prefs without a project open
        return;
    }

    const auto trackeditProject = project->trackeditProject();
    IF_ASSERT_FAILED(trackeditProject) {
        return;
    }
    const spectrogram::AllSpectrogramSettings globalSettings = globalSpectrogramConfiguration()->allSettings();
    for (const Track track : trackeditProject->trackList()) {
        const auto trackConfig = Au3TrackSpectrogramConfiguration::create(track.id, *globalContext());
        IF_ASSERT_FAILED(trackConfig) {
            continue;
        }
        if (trackConfig->useGlobalSettings()) {
            trackConfig->setAllSettings(globalSettings);
        }
    }
}
}
