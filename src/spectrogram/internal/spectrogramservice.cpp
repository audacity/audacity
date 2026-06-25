/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramservice.h"

#include "spectrogramtypes.h"
#include "shared/axis/numberscale.h"
#include "./spectrogramutils.h"
#include "au3/au3trackspectrogramconfiguration.h"
#include "au3/au3spectrogramsettings.h"

#include "au3wrap/internal/domaccessor.h"

#include "au3-project/Project.h"

namespace au::spectrogram {
void SpectrogramService::init()
{
    Au3SpectrogramSettings::setGlobalSpectrogramConfiguration(globalSpectrogramConfiguration());
}

ITrackSpectrogramConfigurationPtr SpectrogramService::trackSpectrogramConfiguration(int trackId) const
{
    return Au3TrackSpectrogramConfiguration::create(trackId, *globalContext());
}

muse::async::Channel<int> SpectrogramService::trackSpectrogramConfigurationChanged() const
{
    return m_trackSpectrogramConfigurationChanged;
}

void SpectrogramService::notifyAboutTrackSpectrogramConfigurationChanged(int trackId)
{
    m_trackSpectrogramConfigurationChanged.send(trackId);
}

void SpectrogramService::copyConfiguration(ISpectrogramConfiguration& source,
                                           ISpectrogramConfiguration& destination) const
{
    destination.setMinFreq(source.minFreq());
    destination.setMaxFreq(source.maxFreq());
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

double SpectrogramService::trackSampleRate(int trackId) const
{
    const auto prj = globalContext()->currentProject();
    // If config is not null, then so should prj.
    IF_ASSERT_FAILED(prj) {
        return 0.;
    }

    au3::Au3WaveTrack* const waveTrack = au3::DomAccessor::findWaveTrack(*reinterpret_cast<::AudacityProject*>(prj->au3ProjectPtr()),
                                                                         ::TrackId { trackId });
    if (!waveTrack) {
        return 0.;
    }

    return waveTrack->GetRate();
}

double SpectrogramService::frequencyHardMaximum(int trackId) const
{
    return trackSampleRate(trackId) / 2;
}

double SpectrogramService::yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const
{
    const auto config = trackSpectrogramConfiguration(trackId);
    if (!config) {
        return SelectionInfo::UndefinedFrequency;
    }

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, trackSampleRate(trackId));
    const NumberScale numberScale{ config->scale(), minFreq, maxFreq };
    return numberScale.positionToValue((spectrogramHeight - spectrogramY) / spectrogramHeight);
}

double SpectrogramService::frequencyToY(int trackId, double frequency, double spectrogramHeight) const
{
    const auto config = trackSpectrogramConfiguration(trackId);
    if (!config) {
        return 0.0;
    }

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, trackSampleRate(trackId));
    const NumberScale numberScale(config->scale(), minFreq, maxFreq);
    const double valuePosition = numberScale.valueToPosition(frequency);
    return (1.0 - valuePosition) * spectrogramHeight;
}
}
