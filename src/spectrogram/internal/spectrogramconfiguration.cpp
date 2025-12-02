/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramconfiguration.h"

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

void SpectrogramConfiguration::init()
{
    muse::settings()->setDefaultValue(SPECTRAL_SELECTION_ENABLED, muse::Val(true));
    muse::settings()->valueChanged(SPECTRAL_SELECTION_ENABLED).onReceive(this, [this](const muse::Val& val) {
        m_spectralSelectionEnabledChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(COLOR_SCHEME, muse::Val(SpectrogramColorScheme::Roseus));
    muse::settings()->valueChanged(COLOR_SCHEME).onReceive(this, [this](const muse::Val& val) {
        m_colorSchemeChanged.send(static_cast<SpectrogramColorScheme>(val.toInt()));
    });

    muse::settings()->setDefaultValue(COLOR_GAIN_DB, muse::Val(20));
    muse::settings()->valueChanged(COLOR_GAIN_DB).onReceive(this, [this](const muse::Val& val) {
        m_colorGainDbChanged.send(val.toInt());
    });

    muse::settings()->setDefaultValue(COLOR_RANGE_DB, muse::Val(80));
    muse::settings()->valueChanged(COLOR_RANGE_DB).onReceive(this, [this](const muse::Val& val) {
        m_colorRangeDbChanged.send(val.toInt());
    });

    muse::settings()->setDefaultValue(COLOR_HIGH_BOOST_DB_PER_DEC, muse::Val(0));
    muse::settings()->valueChanged(COLOR_HIGH_BOOST_DB_PER_DEC).onReceive(this, [this](const muse::Val& val) {
        m_colorHighBoostDbPerDecChanged.send(val.toInt());
    });

    muse::settings()->setDefaultValue(SCALE, muse::Val(static_cast<int>(SpectrogramScale::Mel)));
    muse::settings()->valueChanged(SCALE).onReceive(this, [this](const muse::Val& val) {
        m_scaleChanged.send(val.toEnum<SpectrogramScale>());
    });

    muse::settings()->setDefaultValue(ALGORITHM, muse::Val(SpectrogramAlgorithm::Frequencies));
    muse::settings()->valueChanged(ALGORITHM).onReceive(this, [this](const muse::Val& val) {
        m_algorithmChanged.send(val.toEnum<SpectrogramAlgorithm>());
    });

    muse::settings()->setDefaultValue(WINDOW_TYPE, muse::Val(SpectrogramWindowType::Hann));
    muse::settings()->valueChanged(WINDOW_TYPE).onReceive(this, [this](const muse::Val& val) {
        m_windowTypeChanged.send(val.toEnum<SpectrogramWindowType>());
    });

    muse::settings()->setDefaultValue(WIN_SIZE_LOG2, muse::Val(11 /*2048*/));
    muse::settings()->valueChanged(WIN_SIZE_LOG2).onReceive(this, [this](const muse::Val& val) {
        m_winSizeLog2Changed.send(val.toInt());
    });

    muse::settings()->setDefaultValue(ZERO_PADDING_FACTOR, muse::Val(2));
    muse::settings()->valueChanged(ZERO_PADDING_FACTOR).onReceive(this, [this](const muse::Val& val) {
        m_zeroPaddingFactorChanged.send(val.toInt());
    });
}

bool SpectrogramConfiguration::spectralSelectionEnabled() const
{
    return muse::settings()->value(SPECTRAL_SELECTION_ENABLED).toBool();
}

void SpectrogramConfiguration::setSpectralSelectionEnabled(bool value)
{
    if (spectralSelectionEnabled() == value) {
        return;
    }
    muse::settings()->setSharedValue(SPECTRAL_SELECTION_ENABLED, muse::Val(value));
}

muse::async::Channel<bool> SpectrogramConfiguration::spectralSelectionEnabledChanged() const
{
    return m_spectralSelectionEnabledChanged;
}

SpectrogramColorScheme SpectrogramConfiguration::colorScheme() const
{
    return muse::settings()->value(COLOR_SCHEME).toEnum<SpectrogramColorScheme>();
}

void SpectrogramConfiguration::setColorScheme(SpectrogramColorScheme value)
{
    if (colorScheme() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_SCHEME, muse::Val(value));
}

muse::async::Channel<SpectrogramColorScheme> SpectrogramConfiguration::colorSchemeChanged() const
{
    return m_colorSchemeChanged;
}

int SpectrogramConfiguration::colorGainDb() const
{
    return muse::settings()->value(COLOR_GAIN_DB).toInt();
}

void SpectrogramConfiguration::setColorGainDb(int value)
{
    if (colorGainDb() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_GAIN_DB, muse::Val(value));
}

muse::async::Channel<int> SpectrogramConfiguration::colorGainDbChanged() const
{
    return m_colorGainDbChanged;
}

int SpectrogramConfiguration::colorRangeDb() const
{
    return muse::settings()->value(COLOR_RANGE_DB).toInt();
}

void SpectrogramConfiguration::setColorRangeDb(int value)
{
    if (colorRangeDb() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_RANGE_DB, muse::Val(value));
}

muse::async::Channel<int> SpectrogramConfiguration::colorRangeDbChanged() const
{
    return m_colorRangeDbChanged;
}

int SpectrogramConfiguration::colorHighBoostDbPerDec() const
{
    return muse::settings()->value(COLOR_HIGH_BOOST_DB_PER_DEC).toInt();
}

void SpectrogramConfiguration::setColorHighBoostDbPerDec(int value)
{
    if (colorHighBoostDbPerDec() == value) {
        return;
    }
    muse::settings()->setSharedValue(COLOR_HIGH_BOOST_DB_PER_DEC, muse::Val(value));
}

muse::async::Channel<int> SpectrogramConfiguration::colorHighBoostDbPerDecChanged() const
{
    return m_colorHighBoostDbPerDecChanged;
}

SpectrogramScale SpectrogramConfiguration::scale() const
{
    return muse::settings()->value(SCALE).toEnum<SpectrogramScale>();
}

void SpectrogramConfiguration::setScale(SpectrogramScale value)
{
    if (scale() == value) {
        return;
    }
    muse::settings()->setSharedValue(SCALE, muse::Val(value));
}

muse::async::Channel<SpectrogramScale> SpectrogramConfiguration::scaleChanged() const
{
    return m_scaleChanged;
}

SpectrogramAlgorithm SpectrogramConfiguration::algorithm() const
{
    return muse::settings()->value(ALGORITHM).toEnum<SpectrogramAlgorithm>();
}

void SpectrogramConfiguration::setAlgorithm(SpectrogramAlgorithm value)
{
    if (algorithm() == value) {
        return;
    }
    muse::settings()->setSharedValue(ALGORITHM, muse::Val(value));
}

muse::async::Channel<SpectrogramAlgorithm> SpectrogramConfiguration::algorithmChanged() const
{
    return m_algorithmChanged;
}

SpectrogramWindowType SpectrogramConfiguration::windowType() const
{
    return muse::settings()->value(WINDOW_TYPE).toEnum<SpectrogramWindowType>();
}

void SpectrogramConfiguration::setWindowType(SpectrogramWindowType value)
{
    if (windowType() == value) {
        return;
    }
    muse::settings()->setSharedValue(WINDOW_TYPE, muse::Val(value));
}

muse::async::Channel<SpectrogramWindowType> SpectrogramConfiguration::windowTypeChanged() const
{
    return m_windowTypeChanged;
}

int SpectrogramConfiguration::winSizeLog2() const
{
    return muse::settings()->value(WIN_SIZE_LOG2).toInt();
}

void SpectrogramConfiguration::setWinSizeLog2(int value)
{
    if (winSizeLog2() == value) {
        return;
    }
    muse::settings()->setSharedValue(WIN_SIZE_LOG2, muse::Val(value));
}

muse::async::Channel<int> SpectrogramConfiguration::winSizeLog2Changed() const
{
    return m_winSizeLog2Changed;
}

int SpectrogramConfiguration::zeroPaddingFactor() const
{
    return muse::settings()->value(ZERO_PADDING_FACTOR).toInt();
}

void SpectrogramConfiguration::setZeroPaddingFactor(int value)
{
    if (zeroPaddingFactor() == value) {
        return;
    }
    muse::settings()->setSharedValue(ZERO_PADDING_FACTOR, muse::Val(value));
}

muse::async::Channel<int> SpectrogramConfiguration::zeroPaddingFactorChanged() const
{
    return m_zeroPaddingFactorChanged;
}
}
