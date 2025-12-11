/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogramconfiguration.h"

namespace au::context {
class IGlobalContext;
}

namespace au::spectrogram {
class SpectrogramSettings;
}

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration final : public spectrogram::ISpectrogramConfiguration
{
public:
    static std::shared_ptr<Au3TrackSpectrogramConfiguration> create(int trackId, const context::IGlobalContext& globalContext);

    Au3TrackSpectrogramConfiguration(spectrogram::SpectrogramSettings&);
    ~Au3TrackSpectrogramConfiguration() override = default;

    bool spectralSelectionEnabled() const override;
    void setSpectralSelectionEnabled(bool value) override;

    int colorGainDb() const override;
    void setColorGainDb(int value) override;

    int colorRangeDb() const override;
    void setColorRangeDb(int value) override;

    int colorHighBoostDbPerDec() const override;
    void setColorHighBoostDbPerDec(int value) override;

    spectrogram::SpectrogramColorScheme colorScheme() const override;
    void setColorScheme(spectrogram::SpectrogramColorScheme value) override;

    spectrogram::SpectrogramScale scale() const override;
    void setScale(spectrogram::SpectrogramScale value) override;

    spectrogram::SpectrogramAlgorithm algorithm() const override;
    void setAlgorithm(spectrogram::SpectrogramAlgorithm value) override;

    spectrogram::SpectrogramWindowType windowType() const override;
    void setWindowType(spectrogram::SpectrogramWindowType value) override;

    int winSizeLog2() const override;
    void setWinSizeLog2(int value) override;

    int zeroPaddingFactor() const override;
    void setZeroPaddingFactor(int value) override;

    bool useGlobalSettings() const;
    void setUseGlobalSettings(bool value);

    spectrogram::AllSpectrogramSettings allSettings() const override;
    void setAllSettings(const spectrogram::AllSpectrogramSettings&) override;

private:
    spectrogram::SpectrogramSettings& m_settings;
};
}
