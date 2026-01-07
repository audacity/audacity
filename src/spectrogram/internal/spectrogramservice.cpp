/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramservice.h"

#include "spectrogramtypes.h"
#include "./numberscale.h"
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

void SpectrogramService::copyConfiguration(const ISpectrogramConfiguration& source,
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

double SpectrogramService::yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const
{
    const auto config = trackSpectrogramConfiguration(trackId);
    if (!config) {
        return SelectionInfo::UndefinedFrequency;
    }

    const auto prj = globalContext()->currentProject();
    // If config is not null, then so should prj.
    IF_ASSERT_FAILED(prj) {
        return SelectionInfo::UndefinedFrequency;
    }

    au3::Au3WaveTrack* const waveTrack = au3::DomAccessor::findWaveTrack(*reinterpret_cast<::AudacityProject*>(prj->au3ProjectPtr()),
                                                                         ::TrackId { trackId });

    const auto [minFreq, maxFreq] = spectrogramBounds(*config, waveTrack->GetRate());
    const NumberScale numberScale{ config->scale(), minFreq, maxFreq };
    return std::clamp(numberScale.positionToValue((spectrogramHeight - spectrogramY) / spectrogramHeight),
                      minFreq,
                      maxFreq);
}
}
