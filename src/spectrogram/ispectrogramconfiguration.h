/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"

namespace au::spectrogram {
class ISpectrogramConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramConfiguration)

public:
    virtual ~ISpectrogramConfiguration() = default;

    virtual bool spectralSelectionEnabled() const = 0;
    virtual void setSpectralSelectionEnabled(bool value) = 0;
    virtual muse::async::Channel<bool> spectralSelectionEnabledChanged() const = 0;

    virtual SpectrogramColorScheme colorScheme() const = 0;
    virtual void setColorScheme(SpectrogramColorScheme value) = 0;
    virtual muse::async::Channel<SpectrogramColorScheme> colorSchemeChanged() const = 0;

    virtual int colorGainDb() const = 0;
    virtual void setColorGainDb(int value) = 0;
    virtual muse::async::Channel<int> colorGainDbChanged() const = 0;

    virtual int colorRangeDb() const = 0;
    virtual void setColorRangeDb(int value) = 0;
    virtual muse::async::Channel<int> colorRangeDbChanged() const = 0;

    /**
     * @brief Color-high boost in dB per decave (portmanteau word for "decade" and "octave")
     */
    virtual int colorHighBoostDbPerDec() const = 0;
    virtual void setColorHighBoostDbPerDec(int value) = 0;
    virtual muse::async::Channel<int> colorHighBoostDbPerDecChanged() const = 0;

    virtual SpectrogramScale scale() const = 0;
    virtual void setScale(SpectrogramScale value) = 0;
    virtual muse::async::Channel<SpectrogramScale> scaleChanged() const = 0;

    virtual SpectrogramAlgorithm algorithm() const = 0;
    virtual void setAlgorithm(SpectrogramAlgorithm value) = 0;
    virtual muse::async::Channel<SpectrogramAlgorithm> algorithmChanged() const = 0;

    virtual SpectrogramWindowType windowType() const = 0;
    virtual void setWindowType(SpectrogramWindowType value) = 0;
    virtual muse::async::Channel<SpectrogramWindowType> windowTypeChanged() const = 0;

    virtual int winSizeLog2() const = 0;
    virtual void setWinSizeLog2(int value) = 0;
    virtual muse::async::Channel<int> winSizeLog2Changed() const = 0;

    virtual int zeroPaddingFactor() const = 0;
    virtual void setZeroPaddingFactor(int value) = 0;
    virtual muse::async::Channel<int> zeroPaddingFactorChanged() const = 0;
};
} // namespace au::spectrogram
