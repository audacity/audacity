/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iglobalspectrogramconfiguration.h"

#include "framework/global/async/asyncable.h"

namespace au::spectrogram {
class GlobalSpectrogramConfiguration : public IGlobalSpectrogramConfiguration, public muse::async::Asyncable
{
public:
    ~GlobalSpectrogramConfiguration() override = default;

    void init();

    int minFreq() const override;
    void setMinFreq(int value) override;
    muse::async::Channel<int> minFreqChanged() const;

    int maxFreq() const override;
    void setMaxFreq(int value) override;
    muse::async::Channel<int> maxFreqChanged() const;

    bool spectralSelectionEnabled() const override;
    void setSpectralSelectionEnabled(bool value) override;
    muse::async::Channel<bool> spectralSelectionEnabledChanged() const override;

    SpectrogramColorScheme colorScheme() const override;
    void setColorScheme(SpectrogramColorScheme value) override;
    muse::async::Channel<SpectrogramColorScheme> colorSchemeChanged() const override;

    int colorGainDb() const override;
    void setColorGainDb(int value) override;
    muse::async::Channel<int> colorGainDbChanged() const override;

    int colorRangeDb() const override;
    void setColorRangeDb(int value) override;
    muse::async::Channel<int> colorRangeDbChanged() const override;

    int colorHighBoostDbPerDec() const override;
    void setColorHighBoostDbPerDec(int value) override;
    muse::async::Channel<int> colorHighBoostDbPerDecChanged() const override;

    SpectrogramScale scale() const override;
    void setScale(SpectrogramScale value) override;
    muse::async::Channel<SpectrogramScale> scaleChanged() const override;

    SpectrogramAlgorithm algorithm() const override;
    void setAlgorithm(SpectrogramAlgorithm value) override;
    muse::async::Channel<SpectrogramAlgorithm> algorithmChanged() const override;

    SpectrogramWindowType windowType() const override;
    void setWindowType(SpectrogramWindowType value) override;
    muse::async::Channel<SpectrogramWindowType> windowTypeChanged() const override;

    int winSizeLog2() const override;
    void setWinSizeLog2(int value) override;
    muse::async::Channel<int> winSizeLog2Changed() const override;

    int zeroPaddingFactor() const override;
    void setZeroPaddingFactor(int value) override;
    muse::async::Channel<int> zeroPaddingFactorChanged() const override;

    muse::async::Notification someSettingChanged() const override;

private:
    muse::async::Channel<int> m_minFreqChanged;
    muse::async::Channel<int> m_maxFreqChanged;
    muse::async::Channel<bool> m_spectralSelectionEnabledChanged;
    muse::async::Channel<SpectrogramColorScheme> m_colorSchemeChanged;
    muse::async::Channel<int> m_colorGainDbChanged;
    muse::async::Channel<int> m_colorRangeDbChanged;
    muse::async::Channel<int> m_colorHighBoostDbPerDecChanged;
    muse::async::Channel<SpectrogramScale> m_scaleChanged;
    muse::async::Channel<SpectrogramAlgorithm> m_algorithmChanged;
    muse::async::Channel<SpectrogramWindowType> m_windowTypeChanged;
    muse::async::Channel<int> m_winSizeLog2Changed;
    muse::async::Channel<int> m_zeroPaddingFactorChanged;
    muse::async::Notification m_someSettingChanged;
};
}
