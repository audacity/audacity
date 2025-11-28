/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogramconfiguration.h"

namespace au::trackedit {
class SnapshotSpectrogramConfiguration final : public spectrogram::ISpectrogramConfiguration
{
public:
    SnapshotSpectrogramConfiguration(const ISpectrogramConfiguration& config)
        : m_spectralSelectionEnabled{config.spectralSelectionEnabled()},
        m_colorGainDb{config.colorGainDb()},
        m_colorRangeDb{config.colorRangeDb()},
        m_colorHighBoostDbPerDec{config.colorHighBoostDbPerDec()},
        m_colorScheme{config.colorScheme()},
        m_scale{config.scale()},
        m_algorithm{config.algorithm()},
        m_windowType{config.windowType()},
        m_winSizeLog2{config.winSizeLog2()},
        m_zeroPaddingFactor{config.zeroPaddingFactor()}
    {
    }

    ~SnapshotSpectrogramConfiguration() override = default;

    bool spectralSelectionEnabled() const override { return m_spectralSelectionEnabled; }
    void setSpectralSelectionEnabled(bool) override {}

    int colorGainDb() const override { return m_colorGainDb; }
    void setColorGainDb(int) override {}

    int colorRangeDb() const override { return m_colorRangeDb; }
    void setColorRangeDb(int) override {}

    int colorHighBoostDbPerDec() const override { return m_colorHighBoostDbPerDec; }
    void setColorHighBoostDbPerDec(int) override {}

    spectrogram::SpectrogramColorScheme colorScheme() const override { return m_colorScheme; }
    void setColorScheme(spectrogram::SpectrogramColorScheme) override {}

    spectrogram::SpectrogramScale scale() const override { return m_scale; }
    void setScale(spectrogram::SpectrogramScale) override {}

    spectrogram::SpectrogramAlgorithm algorithm() const override { return m_algorithm; }
    void setAlgorithm(spectrogram::SpectrogramAlgorithm) override {}

    spectrogram::SpectrogramWindowType windowType() const override { return m_windowType; }
    void setWindowType(spectrogram::SpectrogramWindowType) override {}

    int winSizeLog2() const override { return m_winSizeLog2; }
    void setWinSizeLog2(int) override {}

    int zeroPaddingFactor() const override { return m_zeroPaddingFactor; }
    void setZeroPaddingFactor(int) override {}

    spectrogram::AllSpectrogramSettings allSettings() const override
    {
        assert(false);
        return {};
    }

    void setAllSettings(const spectrogram::AllSpectrogramSettings&) {}

private:
    const bool m_spectralSelectionEnabled;
    const int m_colorGainDb;
    const int m_colorRangeDb;
    const int m_colorHighBoostDbPerDec;
    const spectrogram::SpectrogramColorScheme m_colorScheme;
    const spectrogram::SpectrogramScale m_scale;
    const spectrogram::SpectrogramAlgorithm m_algorithm;
    const spectrogram::SpectrogramWindowType m_windowType;
    const int m_winSizeLog2;
    const int m_zeroPaddingFactor;
};
}
