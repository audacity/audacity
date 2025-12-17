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

void TrackSpectrogramConfigurationProvider::copyConfiguration(const ISpectrogramConfiguration& source,
                                                              ISpectrogramConfiguration& destination) const
{
    destination.setColorGainDb(source.colorGainDb());
    destination.setColorRangeDb(source.colorRangeDb());
    destination.setColorHighBoostDbPerDec(source.colorHighBoostDbPerDec());
    destination.setColorScheme(source.colorScheme());
    destination.setScale(source.scale());
    destination.setAlgorithm(source.algorithm());
    destination.setWindowType(source.windowType());
    destination.setWinSizeLog2(source.winSizeLog2());
    destination.setZeroPaddingFactor(source.zeroPaddingFactor());
}
}
