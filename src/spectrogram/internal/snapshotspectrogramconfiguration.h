/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/itrackspectrogramconfiguration.h"

#include <cassert>

namespace au::spectrogram {
class SnapshotSpectrogramConfiguration final : public ITrackSpectrogramConfiguration
{
public:
    SnapshotSpectrogramConfiguration(ITrackSpectrogramConfiguration& config)
        : m_minFreq{config.minFreq()},
        m_maxFreq{config.maxFreq()},
        m_colorGainDb{config.colorGainDb()},
        m_colorRangeDb{config.colorRangeDb()},
        m_colorHighBoostDbPerDec{config.colorHighBoostDbPerDec()},
        m_colorScheme{config.colorScheme()},
        m_scale{config.scale()},
        m_algorithm{config.algorithm()},
        m_windowType{config.windowType()},
        m_winSizeLog2{config.winSizeLog2()},
        m_zeroPaddingFactor{config.zeroPaddingFactor()},
        m_useGlobalSettings{config.useGlobalSettings()}
    {
    }

    ~SnapshotSpectrogramConfiguration() override = default;

    double minFreq() override { return m_minFreq; }
    void setMinFreq(double) override { assert(false); }

    double maxFreq() override { return m_maxFreq; }
    void setMaxFreq(double) override { assert(false); }

    int colorGainDb() override { return m_colorGainDb; }
    void setColorGainDb(int) override { assert(false); }

    int colorRangeDb() override { return m_colorRangeDb; }
    void setColorRangeDb(int) override { assert(false); }

    int colorHighBoostDbPerDec() override { return m_colorHighBoostDbPerDec; }
    void setColorHighBoostDbPerDec(int) override { assert(false); }

    SpectrogramColorScheme colorScheme() override { return m_colorScheme; }
    void setColorScheme(SpectrogramColorScheme) override { assert(false); }

    SpectrogramScale scale() override { return m_scale; }
    void setScale(SpectrogramScale) override { assert(false); }

    SpectrogramAlgorithm algorithm() override { return m_algorithm; }
    void setAlgorithm(SpectrogramAlgorithm) override { assert(false); }

    SpectrogramWindowType windowType() override { return m_windowType; }
    void setWindowType(SpectrogramWindowType) override { assert(false); }

    int winSizeLog2() override { return m_winSizeLog2; }
    void setWinSizeLog2(int) override { assert(false); }

    int zeroPaddingFactor() override { return m_zeroPaddingFactor; }
    void setZeroPaddingFactor(int) override { assert(false); }

    bool useGlobalSettings() override { return m_useGlobalSettings; }
    void setUseGlobalSettings(bool) override { assert(false); }

private:
    const double m_minFreq;
    const double m_maxFreq;
    const int m_colorGainDb;
    const int m_colorRangeDb;
    const int m_colorHighBoostDbPerDec;
    const SpectrogramColorScheme m_colorScheme;
    const SpectrogramScale m_scale;
    const SpectrogramAlgorithm m_algorithm;
    const SpectrogramWindowType m_windowType;
    const int m_winSizeLog2;
    const int m_zeroPaddingFactor;
    const bool m_useGlobalSettings;
};
}
