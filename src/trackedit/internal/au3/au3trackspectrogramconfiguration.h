/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogramconfiguration.h"

namespace au::context {
class IGlobalContext;
}

class SpectrogramSettings;

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration final : public spectrogram::ISpectrogramConfiguration
{
public:
    static std::shared_ptr<Au3TrackSpectrogramConfiguration> create(int trackId, const context::IGlobalContext& globalContext);

    Au3TrackSpectrogramConfiguration(::SpectrogramSettings&);
    ~Au3TrackSpectrogramConfiguration() override = default;

    bool spectralSelectionEnabled() const override;
    void setSpectralSelectionEnabled(bool value) override;
    muse::async::Channel<bool> spectralSelectionEnabledChanged() const override;

    int colorGainDb() const override;
    void setColorGainDb(int value) override;
    muse::async::Channel<int> colorGainDbChanged() const override;

    int colorRangeDb() const override;
    void setColorRangeDb(int value) override;
    muse::async::Channel<int> colorRangeDbChanged() const override;

    int colorHighBoostDbPerDec() const override;
    void setColorHighBoostDbPerDec(int value) override;
    muse::async::Channel<int> colorHighBoostDbPerDecChanged() const override;

    spectrogram::SpectrogramColorScheme colorScheme() const override;
    void setColorScheme(spectrogram::SpectrogramColorScheme value) override;
    muse::async::Channel<spectrogram::SpectrogramColorScheme> colorSchemeChanged() const override;

    spectrogram::SpectrogramScale scale() const override;
    void setScale(spectrogram::SpectrogramScale value) override;
    muse::async::Channel<spectrogram::SpectrogramScale> scaleChanged() const override;

    spectrogram::SpectrogramAlgorithm algorithm() const override;
    void setAlgorithm(spectrogram::SpectrogramAlgorithm value) override;
    muse::async::Channel<spectrogram::SpectrogramAlgorithm> algorithmChanged() const override;

    spectrogram::SpectrogramWindowType windowType() const override;
    void setWindowType(spectrogram::SpectrogramWindowType value) override;
    muse::async::Channel<spectrogram::SpectrogramWindowType> windowTypeChanged() const override;

    int winSizeLog2() const override;
    void setWinSizeLog2(int value) override;
    muse::async::Channel<int> winSizeLog2Changed() const override;

    int zeroPaddingFactor() const override;
    void setZeroPaddingFactor(int value) override;
    muse::async::Channel<int> zeroPaddingFactorChanged() const override;

    bool useGlobalSettings() const;
    void setUseGlobalSettings(bool value);

    spectrogram::AllSpectrogramSettings allSettings() const override;
    void setAllSettings(const spectrogram::AllSpectrogramSettings&) override;
    muse::async::Notification someSettingChanged() const override;

private:
    muse::async::Channel<bool> m_spectralSelectionEnabledChanged;
    muse::async::Channel<int> m_colorGainDbChanged;
    muse::async::Channel<int> m_colorRangeDbChanged;
    muse::async::Channel<int> m_colorHighBoostDbPerDecChanged;
    muse::async::Channel<spectrogram::SpectrogramColorScheme> m_colorSchemeChanged;
    muse::async::Channel<spectrogram::SpectrogramScale> m_scaleChanged;
    muse::async::Channel<spectrogram::SpectrogramAlgorithm> m_algorithmChanged;
    muse::async::Channel<spectrogram::SpectrogramWindowType> m_windowTypeChanged;
    muse::async::Channel<int> m_winSizeLog2Changed;
    muse::async::Channel<int> m_zeroPaddingFactorChanged;
    muse::async::Notification m_someSettingChanged;

    ::SpectrogramSettings& m_settings;
};
}
