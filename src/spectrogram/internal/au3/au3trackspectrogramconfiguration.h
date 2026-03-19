/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfiguration.h"

#include <memory>

class AudacityProject;
class WaveTrack;

namespace au::context {
class IGlobalContext;
}

namespace au::spectrogram {
class Au3SpectrogramSettings;

class Au3TrackSpectrogramConfiguration final : public ITrackSpectrogramConfiguration
{
public:
    static std::shared_ptr<Au3TrackSpectrogramConfiguration> create(int trackId, const context::IGlobalContext& globalContext);

    Au3TrackSpectrogramConfiguration(int trackId, AudacityProject& project);
    ~Au3TrackSpectrogramConfiguration() override = default;

    double minFreq() override;
    void setMinFreq(double value) override;

    double maxFreq() override;
    void setMaxFreq(double value) override;

    int colorGainDb() override;
    void setColorGainDb(int value) override;

    int colorRangeDb() override;
    void setColorRangeDb(int value) override;

    int colorHighBoostDbPerDec() override;
    void setColorHighBoostDbPerDec(int value) override;

    SpectrogramColorScheme colorScheme() override;
    void setColorScheme(SpectrogramColorScheme value) override;

    SpectrogramScale scale() override;
    void setScale(SpectrogramScale value) override;

    SpectrogramAlgorithm algorithm() override;
    void setAlgorithm(SpectrogramAlgorithm value) override;

    SpectrogramWindowType windowType() override;
    void setWindowType(SpectrogramWindowType value) override;

    int winSizeLog2() override;
    void setWinSizeLog2(int value) override;

    int zeroPaddingFactor() override;
    void setZeroPaddingFactor(int value) override;

    bool useGlobalSettings() override;
    void setUseGlobalSettings(bool value) override;

private:
    bool maybeReloadSettings();

    const int m_trackId;
    std::weak_ptr<AudacityProject> m_weakProject;
    std::weak_ptr<WaveTrack> m_weakWaveTrack;
    Au3SpectrogramSettings* m_settings = nullptr;
};
}
