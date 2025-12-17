/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramconfigurationprovider.h"

#include "au3/au3trackspectrogramconfiguration.h"
#include "internal/au3/au3spectrogramsettings.h"

namespace au::spectrogram {
void TrackSpectrogramConfigurationProvider::init()
{
    Au3SpectrogramSettings::setGlobalSpectrogramConfiguration(globalSpectrogramConfiguration());
}

ITrackSpectrogramConfigurationPtr TrackSpectrogramConfigurationProvider::trackSpectrogramConfiguration(int trackId) const
{
    return Au3TrackSpectrogramConfiguration::create(trackId, *globalContext());
}
}
