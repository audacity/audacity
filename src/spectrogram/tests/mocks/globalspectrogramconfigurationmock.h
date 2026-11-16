/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "spectrogram/iglobalspectrogramconfiguration.h"

namespace au::spectrogram {
class GlobalSpectrogramConfigurationMock : public IGlobalSpectrogramConfiguration
{
public:
    MOCK_METHOD(double, minFreq, (), (override));
    MOCK_METHOD(void, setMinFreq, (double), (override));

    MOCK_METHOD(double, maxFreq, (), (override));
    MOCK_METHOD(void, setMaxFreq, (double), (override));

    MOCK_METHOD(SpectrogramColorScheme, colorScheme, (), (override));
    MOCK_METHOD(void, setColorScheme, (SpectrogramColorScheme), (override));

    MOCK_METHOD(int, colorGainDb, (), (override));
    MOCK_METHOD(void, setColorGainDb, (int), (override));

    MOCK_METHOD(int, colorRangeDb, (), (override));
    MOCK_METHOD(void, setColorRangeDb, (int), (override));

    MOCK_METHOD(int, colorHighBoostDbPerDec, (), (override));
    MOCK_METHOD(void, setColorHighBoostDbPerDec, (int), (override));

    MOCK_METHOD(SpectrogramScale, scale, (), (override));
    MOCK_METHOD(void, setScale, (SpectrogramScale), (override));

    MOCK_METHOD(SpectrogramAlgorithm, algorithm, (), (override));
    MOCK_METHOD(void, setAlgorithm, (SpectrogramAlgorithm), (override));

    MOCK_METHOD(SpectrogramWindowType, windowType, (), (override));
    MOCK_METHOD(void, setWindowType, (SpectrogramWindowType), (override));

    MOCK_METHOD(int, winSizeLog2, (), (override));
    MOCK_METHOD(void, setWinSizeLog2, (int), (override));

    MOCK_METHOD(int, zeroPaddingFactor, (), (override));
    MOCK_METHOD(void, setZeroPaddingFactor, (int), (override));

    MOCK_METHOD(bool, spectralSelectionEnabled, (), (const, override));
    MOCK_METHOD(void, setSpectralSelectionEnabled, (bool), (override));
    MOCK_METHOD(muse::async::Channel<bool>, spectralSelectionEnabledChanged, (), (const, override));

    MOCK_METHOD(muse::async::Channel<double>, minFreqChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<double>, maxFreqChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<SpectrogramColorScheme>, colorSchemeChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<int>, colorGainDbChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<int>, colorRangeDbChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<int>, colorHighBoostDbPerDecChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<SpectrogramScale>, scaleChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<SpectrogramAlgorithm>, algorithmChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<SpectrogramWindowType>, windowTypeChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<int>, winSizeLog2Changed, (), (const, override));
    MOCK_METHOD(muse::async::Channel<int>, zeroPaddingFactorChanged, (), (const, override));
    MOCK_METHOD(muse::async::Notification, someSettingChanged, (), (const, override));
};
}
