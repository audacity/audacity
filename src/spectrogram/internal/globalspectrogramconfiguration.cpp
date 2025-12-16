/*
 * Audacity: A Digital Audio Editor
 */
#include "globalspectrogramconfiguration.h"

#include "framework/global/settings.h"

namespace au::spectrogram {
namespace {
static const std::string moduleName("spectrogram");

static const muse::Settings::Key SPECTRAL_SELECTION_ENABLED(moduleName, "spectrogram/spectralSelectionEnabled");
static const muse::Settings::Key COLOR_SCHEME(moduleName, "spectrogram/colorScheme");
static const muse::Settings::Key COLOR_GAIN_DB(moduleName, "spectrogram/colorGainDb");
static const muse::Settings::Key COLOR_RANGE_DB(moduleName, "spectrogram/colorRangeDb");
static const muse::Settings::Key COLOR_HIGH_BOOST_DB_PER_DEC(moduleName, "spectrogram/colorHighBoostDbPerDec");
static const muse::Settings::Key SCALE(moduleName, "spectrogram/scale");
static const muse::Settings::Key ALGORITHM(moduleName, "spectrogram/algorithm");
static const muse::Settings::Key WINDOW_TYPE(moduleName, "spectrogram/windowType");
static const muse::Settings::Key WIN_SIZE_LOG2(moduleName, "spectrogram/winSizeLog2");
static const muse::Settings::Key ZERO_PADDING_FACTOR(moduleName, "spectrogram/zeroPaddingFactor");
}

void GlobalSpectrogramConfiguration::init()
{
    muse::settings()->setDefaultValue(SPECTRAL_SELECTION_ENABLED, muse::Val(true));
    muse::settings()->valueChanged(SPECTRAL_SELECTION_ENABLED).onReceive(this, [this](const muse::Val& val) {
        m_spectralSelectionEnabledChanged.send(val.toBool());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(COLOR_SCHEME, muse::Val(SpectrogramColorScheme::Roseus));
    muse::settings()->valueChanged(COLOR_SCHEME).onReceive(this, [this](const muse::Val& val) {
        m_colorSchemeChanged.send(static_cast<SpectrogramColorScheme>(val.toInt()));
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(COLOR_GAIN_DB, muse::Val(20));
    muse::settings()->valueChanged(COLOR_GAIN_DB).onReceive(this, [this](const muse::Val& val) {
        m_colorGainDbChanged.send(val.toInt());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(COLOR_RANGE_DB, muse::Val(80));
    muse::settings()->valueChanged(COLOR_RANGE_DB).onReceive(this, [this](const muse::Val& val) {
        m_colorRangeDbChanged.send(val.toInt());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(COLOR_HIGH_BOOST_DB_PER_DEC, muse::Val(0));
    muse::settings()->valueChanged(COLOR_HIGH_BOOST_DB_PER_DEC).onReceive(this, [this](const muse::Val& val) {
        m_colorHighBoostDbPerDecChanged.send(val.toInt());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(SCALE, muse::Val(static_cast<int>(SpectrogramScale::Mel)));
    muse::settings()->valueChanged(SCALE).onReceive(this, [this](const muse::Val& val) {
        m_scaleChanged.send(val.toEnum<SpectrogramScale>());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(ALGORITHM, muse::Val(SpectrogramAlgorithm::Frequencies));
    muse::settings()->valueChanged(ALGORITHM).onReceive(this, [this](const muse::Val& val) {
        m_algorithmChanged.send(val.toEnum<SpectrogramAlgorithm>());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(WINDOW_TYPE, muse::Val(SpectrogramWindowType::Hann));
    muse::settings()->valueChanged(WINDOW_TYPE).onReceive(this, [this](const muse::Val& val) {
        m_windowTypeChanged.send(val.toEnum<SpectrogramWindowType>());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(WIN_SIZE_LOG2, muse::Val(11 /*2048*/));
    muse::settings()->valueChanged(WIN_SIZE_LOG2).onReceive(this, [this](const muse::Val& val) {
        m_winSizeLog2Changed.send(val.toInt());
        m_someSettingChanged.notify();
    });

    muse::settings()->setDefaultValue(ZERO_PADDING_FACTOR, muse::Val(2));
    muse::settings()->valueChanged(ZERO_PADDING_FACTOR).onReceive(this, [this](const muse::Val& val) {
        m_zeroPaddingFactorChanged.send(val.toInt());
        m_someSettingChanged.notify();
    });
}

bool GlobalSpectrogramConfiguration::spectralSelectionEnabled() const
{
    return muse::settings()->value(SPECTRAL_SELECTION_ENABLED).toBool();
}

void GlobalSpectrogramConfiguration::setSpectralSelectionEnabled(bool value)
{
    if (spectralSelectionEnabled() == value) {
        return;
    }
    muse::settings()->setSharedValue(SPECTRAL_SELECTION_ENABLED, muse::Val(value));
}

muse::async::Channel<bool> GlobalSpectrogramConfiguration::spectralSelectionEnabledChanged() const
{
    return m_spectralSelectionEnabledChanged;
}

SpectrogramColorScheme GlobalSpectrogramConfiguration::colorScheme() const
{
    return muse::settings()->value(COLOR_SCHEME).toEnum<SpectrogramColorScheme>();
}

void GlobalSpectrogramConfiguration::setColorScheme(SpectrogramColorScheme value)
{
    if (colorScheme() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_SCHEME, muse::Val(value));
}

muse::async::Channel<SpectrogramColorScheme> GlobalSpectrogramConfiguration::colorSchemeChanged() const
{
    return m_colorSchemeChanged;
}

int GlobalSpectrogramConfiguration::colorGainDb() const
{
    return muse::settings()->value(COLOR_GAIN_DB).toInt();
}

void GlobalSpectrogramConfiguration::setColorGainDb(int value)
{
    if (colorGainDb() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_GAIN_DB, muse::Val(value));
}

muse::async::Channel<int> GlobalSpectrogramConfiguration::colorGainDbChanged() const
{
    return m_colorGainDbChanged;
}

int GlobalSpectrogramConfiguration::colorRangeDb() const
{
    return muse::settings()->value(COLOR_RANGE_DB).toInt();
}

void GlobalSpectrogramConfiguration::setColorRangeDb(int value)
{
    if (colorRangeDb() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_RANGE_DB, muse::Val(value));
}

muse::async::Channel<int> GlobalSpectrogramConfiguration::colorRangeDbChanged() const
{
    return m_colorRangeDbChanged;
}

int GlobalSpectrogramConfiguration::colorHighBoostDbPerDec() const
{
    return muse::settings()->value(COLOR_HIGH_BOOST_DB_PER_DEC).toInt();
}

void GlobalSpectrogramConfiguration::setColorHighBoostDbPerDec(int value)
{
    if (colorHighBoostDbPerDec() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_HIGH_BOOST_DB_PER_DEC, muse::Val(value));
}

muse::async::Channel<int> GlobalSpectrogramConfiguration::colorHighBoostDbPerDecChanged() const
{
    return m_colorHighBoostDbPerDecChanged;
}

SpectrogramScale GlobalSpectrogramConfiguration::scale() const
{
    return muse::settings()->value(SCALE).toEnum<SpectrogramScale>();
}

void GlobalSpectrogramConfiguration::setScale(SpectrogramScale value)
{
    if (scale() == value) {
        return;
    }
    muse::settings()->setSharedValue(SCALE, muse::Val(value));
}

muse::async::Channel<SpectrogramScale> GlobalSpectrogramConfiguration::scaleChanged() const
{
    return m_scaleChanged;
}

SpectrogramAlgorithm GlobalSpectrogramConfiguration::algorithm() const
{
    return muse::settings()->value(ALGORITHM).toEnum<SpectrogramAlgorithm>();
}

void GlobalSpectrogramConfiguration::setAlgorithm(SpectrogramAlgorithm value)
{
    if (algorithm() == value) {
        return;
    }
    muse::settings()->setSharedValue(ALGORITHM, muse::Val(value));
}

muse::async::Channel<SpectrogramAlgorithm> GlobalSpectrogramConfiguration::algorithmChanged() const
{
    return m_algorithmChanged;
}

SpectrogramWindowType GlobalSpectrogramConfiguration::windowType() const
{
    return muse::settings()->value(WINDOW_TYPE).toEnum<SpectrogramWindowType>();
}

void GlobalSpectrogramConfiguration::setWindowType(SpectrogramWindowType value)
{
    if (windowType() == value) {
        return;
    }
    muse::settings()->setSharedValue(WINDOW_TYPE, muse::Val(value));
}

muse::async::Channel<SpectrogramWindowType> GlobalSpectrogramConfiguration::windowTypeChanged() const
{
    return m_windowTypeChanged;
}

int GlobalSpectrogramConfiguration::winSizeLog2() const
{
    return muse::settings()->value(WIN_SIZE_LOG2).toInt();
}

void GlobalSpectrogramConfiguration::setWinSizeLog2(int value)
{
    if (winSizeLog2() == value) {
        return;
    }
    muse::settings()->setSharedValue(WIN_SIZE_LOG2, muse::Val(value));
}

muse::async::Channel<int> GlobalSpectrogramConfiguration::winSizeLog2Changed() const
{
    return m_winSizeLog2Changed;
}

int GlobalSpectrogramConfiguration::zeroPaddingFactor() const
{
    return muse::settings()->value(ZERO_PADDING_FACTOR).toInt();
}

void GlobalSpectrogramConfiguration::setZeroPaddingFactor(int value)
{
    if (zeroPaddingFactor() == value) {
        return;
    }
    muse::settings()->setSharedValue(ZERO_PADDING_FACTOR, muse::Val(value));
}

muse::async::Channel<int> GlobalSpectrogramConfiguration::zeroPaddingFactorChanged() const
{
    return m_zeroPaddingFactorChanged;
}

muse::async::Notification GlobalSpectrogramConfiguration::someSettingChanged() const
{
    return m_someSettingChanged;
}
}
