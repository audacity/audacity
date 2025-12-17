/*
 * Audacity: A Digital Audio Editor
 */
#include "globalspectrogramsettingsmodel.h"

namespace au::spectrogram {
GlobalSpectrogramSettingsModel::GlobalSpectrogramSettingsModel(QObject* parent)
    : spectrogram::AbstractSpectrogramSettingsModel(parent)
{}

void GlobalSpectrogramSettingsModel::componentComplete()
{
    configuration()->spectralSelectionEnabledChanged().onReceive(this, [this](auto){
        emit spectralSelectionEnabledChanged();
    });
    configuration()->colorGainDbChanged().onReceive(this, [this](auto){
        emit colorGainDbChanged();
    });
    configuration()->colorRangeDbChanged().onReceive(this, [this](auto){
        emit colorRangeDbChanged();
    });
    configuration()->colorHighBoostDbPerDecChanged().onReceive(this, [this](auto){
        emit colorHighBoostDbPerDecChanged();
    });
    configuration()->colorSchemeChanged().onReceive(this, [this](auto){
        emit colorSchemeChanged();
    });
    configuration()->scaleChanged().onReceive(this, [this](auto){
        emit scaleChanged();
    });
    configuration()->algorithmChanged().onReceive(this, [this](auto){
        emit algorithmChanged();
    });
    configuration()->windowTypeChanged().onReceive(this, [this](auto){
        emit windowTypeChanged();
    });
    configuration()->winSizeLog2Changed().onReceive(this, [this](auto){
        emit windowSizeChanged();
    });
    configuration()->zeroPaddingFactorChanged().onReceive(this, [this](auto){
        emit zeroPaddingFactorChanged();
    });
}

bool GlobalSpectrogramSettingsModel::spectralSelectionEnabled() const
{
    return configuration()->spectralSelectionEnabled();
}

void GlobalSpectrogramSettingsModel::setSpectralSelectionEnabled(bool value)
{
    configuration()->setSpectralSelectionEnabled(value);
}

int GlobalSpectrogramSettingsModel::colorGainDb() const
{
    return configuration()->colorGainDb();
}

void GlobalSpectrogramSettingsModel::setColorGainDb(int value)
{
    configuration()->setColorGainDb(value);
}

int GlobalSpectrogramSettingsModel::colorRangeDb() const
{
    return configuration()->colorRangeDb();
}

void GlobalSpectrogramSettingsModel::setColorRangeDb(int value)
{
    configuration()->setColorRangeDb(value);
}

int GlobalSpectrogramSettingsModel::colorHighBoostDbPerDec() const
{
    return configuration()->colorHighBoostDbPerDec();
}

void GlobalSpectrogramSettingsModel::setColorHighBoostDbPerDec(int value)
{
    configuration()->setColorHighBoostDbPerDec(value);
}

int GlobalSpectrogramSettingsModel::colorScheme() const
{
    return static_cast<int>(configuration()->colorScheme());
}

void GlobalSpectrogramSettingsModel::setColorScheme(int value)
{
    configuration()->setColorScheme(static_cast<spectrogram::SpectrogramColorScheme>(value));
}

int GlobalSpectrogramSettingsModel::scale() const
{
    return static_cast<int>(configuration()->scale());
}

void GlobalSpectrogramSettingsModel::setScale(int value)
{
    configuration()->setScale(static_cast<spectrogram::SpectrogramScale>(value));
}

int GlobalSpectrogramSettingsModel::algorithm() const
{
    return static_cast<int>(configuration()->algorithm());
}

void GlobalSpectrogramSettingsModel::setAlgorithm(int value)
{
    configuration()->setAlgorithm(static_cast<spectrogram::SpectrogramAlgorithm>(value));
}

int GlobalSpectrogramSettingsModel::windowType() const
{
    return static_cast<int>(configuration()->windowType());
}

void GlobalSpectrogramSettingsModel::setWindowType(int value)
{
    configuration()->setWindowType(static_cast<spectrogram::SpectrogramWindowType>(value));
}

int GlobalSpectrogramSettingsModel::windowSize() const
{
    return 1 << configuration()->winSizeLog2();
}

void GlobalSpectrogramSettingsModel::setWindowSize(int value)
{
    configuration()->setWinSizeLog2(log2(value));
}

int GlobalSpectrogramSettingsModel::zeroPaddingFactor() const
{
    return configuration()->zeroPaddingFactor();
}

void GlobalSpectrogramSettingsModel::setZeroPaddingFactor(int value)
{
    configuration()->setZeroPaddingFactor(value);
}
} // namespace au::spectrogram
