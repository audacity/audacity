/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramconfigurationprovider.h"

#include "au3/au3trackspectrogramconfiguration.h"

namespace au::spectrogram {
ITrackSpectrogramConfigurationPtr TrackSpectrogramConfigurationProvider::trackSpectrogramConfiguration(int trackId) const
{
  return Au3TrackSpectrogramConfiguration::create(trackId, *globalContext());
}
}
