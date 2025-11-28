/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"

namespace au::spectrogram {
class ISpectrogramConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramConfiguration)

public:
    virtual ~ISpectrogramConfiguration() = default;

    virtual bool spectralSelectionEnabled() const = 0;
    virtual void setSpectralSelectionEnabled(bool value) = 0;
    virtual muse::async::Channel<bool> spectralSelectionEnabledChanged() const { return {}; }

    virtual SpectrogramColorScheme colorScheme() const = 0;
    virtual void setColorScheme(SpectrogramColorScheme value) = 0;
    virtual muse::async::Channel<SpectrogramColorScheme> colorSchemeChanged() const { return {}; }

    virtual int colorGainDb() const = 0;
    virtual void setColorGainDb(int value) = 0;
    virtual muse::async::Channel<int> colorGainDbChanged() const { return {}; }

    virtual int colorRangeDb() const = 0;
    virtual void setColorRangeDb(int value) = 0;
    virtual muse::async::Channel<int> colorRangeDbChanged() const { return {}; }

    /**
     * @brief Color-high boost in dB per decave (portmanteau word for "decade" and "octave")
     */
    virtual int colorHighBoostDbPerDec() const = 0;
    virtual void setColorHighBoostDbPerDec(int value) = 0;
    virtual muse::async::Channel<int> colorHighBoostDbPerDecChanged() const { return {}; }

    virtual SpectrogramScale scale() const = 0;
    virtual void setScale(SpectrogramScale value) = 0;
    virtual muse::async::Channel<SpectrogramScale> scaleChanged() const { return {}; }

    virtual SpectrogramAlgorithm algorithm() const = 0;
    virtual void setAlgorithm(SpectrogramAlgorithm value) = 0;
    virtual muse::async::Channel<SpectrogramAlgorithm> algorithmChanged() const { return {}; }

    virtual SpectrogramWindowType windowType() const = 0;
    virtual void setWindowType(SpectrogramWindowType value) = 0;
    virtual muse::async::Channel<SpectrogramWindowType> windowTypeChanged() const { return {}; }

    virtual int winSizeLog2() const = 0;
    virtual void setWinSizeLog2(int value) = 0;
    virtual muse::async::Channel<int> winSizeLog2Changed() const { return {}; }

    virtual int zeroPaddingFactor() const = 0;
    virtual void setZeroPaddingFactor(int value) = 0;
    virtual muse::async::Channel<int> zeroPaddingFactorChanged() const { return {}; }

    virtual AllSpectrogramSettings allSettings() const = 0;
    virtual void setAllSettings(const AllSpectrogramSettings&) = 0;
    virtual muse::async::Notification someSettingChanged() const { return {}; }
};
} // namespace au::spectrogram
