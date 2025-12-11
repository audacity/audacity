/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

namespace au::spectrogram {
class ISpectrogramConfiguration
{
public:
    virtual ~ISpectrogramConfiguration() = default;

    virtual bool spectralSelectionEnabled() const = 0;
    virtual void setSpectralSelectionEnabled(bool value) = 0;

    virtual SpectrogramColorScheme colorScheme() const = 0;
    virtual void setColorScheme(SpectrogramColorScheme value) = 0;

    virtual int colorGainDb() const = 0;
    virtual void setColorGainDb(int value) = 0;

    virtual int colorRangeDb() const = 0;
    virtual void setColorRangeDb(int value) = 0;

    /**
     * @brief Color-high boost in dB per decave (portmanteau word for "decade" and "octave")
     */
    virtual int colorHighBoostDbPerDec() const = 0;
    virtual void setColorHighBoostDbPerDec(int value) = 0;

    virtual SpectrogramScale scale() const = 0;
    virtual void setScale(SpectrogramScale value) = 0;

    virtual SpectrogramAlgorithm algorithm() const = 0;
    virtual void setAlgorithm(SpectrogramAlgorithm value) = 0;

    virtual SpectrogramWindowType windowType() const = 0;
    virtual void setWindowType(SpectrogramWindowType value) = 0;

    virtual int winSizeLog2() const = 0;
    virtual void setWinSizeLog2(int value) = 0;

    virtual int zeroPaddingFactor() const = 0;
    virtual void setZeroPaddingFactor(int value) = 0;

    virtual AllSpectrogramSettings allSettings() const = 0;
    virtual void setAllSettings(const AllSpectrogramSettings&) = 0;
};
} // namespace au::spectrogram
