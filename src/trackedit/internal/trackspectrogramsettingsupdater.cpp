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

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]{
        maybeApplyGlobalSettingsToTrack();
    });

    maybeApplyGlobalSettingsToTrack();
}

void TrackSpectrogramSettingsUpdater::maybeApplyGlobalSettingsToTrack()
{
    forEachTrack([this](ITrackeditProject& trackeditProject, const Track& track, spectrogram::ITrackSpectrogramConfiguration& config){
        if (config.useGlobalSettings()) {
            spectrogramService()->copyConfiguration(*globalSpectrogramConfiguration(), config);
            trackeditProject.notifyAboutTrackChanged(track);
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
        if (track.type != TrackType::Mono && track.type != TrackType::Stereo) {
            continue;
        }
        const auto trackConfig = spectrogramService()->trackSpectrogramConfiguration(track.id);
        IF_ASSERT_FAILED(trackConfig) {
            continue;
        }
        func(*trackeditProject, track, *trackConfig);
    }
}
}
