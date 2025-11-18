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
        emit spectralSelectionEnabledChanged_1();
    });
    configuration()->colorGainDbChanged().onReceive(this, [this](auto){
        emit colorGainDbChanged_2();
    });
    configuration()->colorRangeDbChanged().onReceive(this, [this](auto){
        emit colorRangeDbChanged_3();
    });
    configuration()->colorHighBoostDbPerDecChanged().onReceive(this, [this](auto){
        emit colorHighBoostDbPerDecChanged_4();
    });
    configuration()->colorSchemeChanged().onReceive(this, [this](auto){
        emit colorSchemeChanged_5();
    });
    configuration()->scaleChanged().onReceive(this, [this](auto){
        emit scaleChanged_6();
    });
    configuration()->algorithmChanged().onReceive(this, [this](auto){
        emit algorithmChanged_7();
    });
    configuration()->windowTypeChanged().onReceive(this, [this](auto){
        emit windowTypeChanged_8();
    });
    configuration()->winSizeLog2Changed().onReceive(this, [this](auto){
        emit windowSizeChanged_9();
    });
    configuration()->zeroPaddingFactorChanged().onReceive(this, [this](auto){
        emit zeroPaddingFactorChanged_10();
    });
}

bool GlobalSpectrogramSettingsModel::spectralSelectionEnabled_1() const
{
    return configuration()->spectralSelectionEnabled();
}

void GlobalSpectrogramSettingsModel::setSpectralSelectionEnabled_1(bool value)
{
    configuration()->setSpectralSelectionEnabled(value);
}

int GlobalSpectrogramSettingsModel::colorGainDb_2() const
{
    return configuration()->colorGainDb();
}

void GlobalSpectrogramSettingsModel::setColorGainDb_2(int value)
{
    configuration()->setColorGainDb(value);
}

int GlobalSpectrogramSettingsModel::colorRangeDb_3() const
{
    return configuration()->colorRangeDb();
}

void GlobalSpectrogramSettingsModel::setColorRangeDb_3(int value)
{
    configuration()->setColorRangeDb(value);
}

int GlobalSpectrogramSettingsModel::colorHighBoostDbPerDec_4() const
{
    return configuration()->colorHighBoostDbPerDec();
}

void GlobalSpectrogramSettingsModel::setColorHighBoostDbPerDec_4(int value)
{
    configuration()->setColorHighBoostDbPerDec(value);
}

int GlobalSpectrogramSettingsModel::colorScheme_5() const
{
    return static_cast<int>(configuration()->colorScheme());
}

void GlobalSpectrogramSettingsModel::setColorScheme_5(int value)
{
    configuration()->setColorScheme(static_cast<spectrogram::SpectrogramColorScheme>(value));
}

int GlobalSpectrogramSettingsModel::scale_6() const
{
    return static_cast<int>(configuration()->scale());
}

void GlobalSpectrogramSettingsModel::setScale_6(int value)
{
    configuration()->setScale(static_cast<spectrogram::SpectrogramScale>(value));
}

int GlobalSpectrogramSettingsModel::algorithm_7() const
{
    return static_cast<int>(configuration()->algorithm());
}

void GlobalSpectrogramSettingsModel::setAlgorithm_7(int value)
{
    configuration()->setAlgorithm(static_cast<spectrogram::SpectrogramAlgorithm>(value));
}

int GlobalSpectrogramSettingsModel::windowType_8() const
{
    return static_cast<int>(configuration()->windowType());
}

void GlobalSpectrogramSettingsModel::setWindowType_8(int value)
{
    configuration()->setWindowType(static_cast<spectrogram::SpectrogramWindowType>(value));
}

int GlobalSpectrogramSettingsModel::windowSize_9() const
{
    return 1 << configuration()->winSizeLog2();
}

void GlobalSpectrogramSettingsModel::setWindowSize_9(int value)
{
    configuration()->setWinSizeLog2(log2(value));
}

int GlobalSpectrogramSettingsModel::zeroPaddingFactor_10() const
{
    return configuration()->zeroPaddingFactor();
}

void GlobalSpectrogramSettingsModel::setZeroPaddingFactor_10(int value)
{
    configuration()->setZeroPaddingFactor(value);
}
} // namespace au::spectrogram
