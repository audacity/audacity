/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfiguration.h"

namespace au::context {
class IGlobalContext;
}

namespace au::spectrogram {
class Au3SpectrogramSettings;

class Au3TrackSpectrogramConfiguration final : public ITrackSpectrogramConfiguration
{
public:
    static std::shared_ptr<Au3TrackSpectrogramConfiguration> create(int trackId, const context::IGlobalContext& globalContext);

    Au3TrackSpectrogramConfiguration(Au3SpectrogramSettings&);
    ~Au3TrackSpectrogramConfiguration() override = default;

    int minFreq() const override;
    void setMinFreq(int value) override;

    int maxFreq() const override;
    void setMaxFreq(int value) override;

    int colorGainDb() const override;
    void setColorGainDb(int value) override;

    int colorRangeDb() const override;
    void setColorRangeDb(int value) override;

    int colorHighBoostDbPerDec() const override;
    void setColorHighBoostDbPerDec(int value) override;

    SpectrogramColorScheme colorScheme() const override;
    void setColorScheme(SpectrogramColorScheme value) override;

    SpectrogramScale scale() const override;
    void setScale(SpectrogramScale value) override;

    SpectrogramAlgorithm algorithm() const override;
    void setAlgorithm(SpectrogramAlgorithm value) override;

    SpectrogramWindowType windowType() const override;
    void setWindowType(SpectrogramWindowType value) override;

    int winSizeLog2() const override;
    void setWinSizeLog2(int value) override;

    int zeroPaddingFactor() const override;
    void setZeroPaddingFactor(int value) override;

    bool useGlobalSettings() const override;
    void setUseGlobalSettings(bool value) override;

private:
    Au3SpectrogramSettings& m_settings;
};
}
