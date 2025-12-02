/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramsettingsmodel.h"
#include "internal/au3/au3trackspectrogramconfiguration.h"

#include "framework/global/defer.h"
#include "framework/global/log.h"

namespace au::trackedit {
TrackSpectrogramSettingsModel::TrackSpectrogramSettingsModel(QObject* parent)
    : spectrogram::AbstractSpectrogramSettingsModel(parent)
{}

void TrackSpectrogramSettingsModel::componentComplete()
{
    const auto trackConfig = Au3TrackSpectrogramConfiguration::create(m_trackId, *globalContext());
    IF_ASSERT_FAILED(trackConfig) {
        return;
    }
    doSetUseGlobalSettings(trackConfig->useGlobalSettings());
    readFromConfig(trackConfig->useGlobalSettings() ? *globalSpectrogramConfiguration() : *trackConfig);
}

void TrackSpectrogramSettingsModel::readFromConfig(const spectrogram::ISpectrogramConfiguration& config)
{
    doSetSpectralSelectionEnabled_1(config.spectralSelectionEnabled(), false);
    doSetColorGainDb_2(config.colorGainDb(), false);
    doSetColorRangeDb_3(config.colorRangeDb(), false);
    doSetColorHighBoostDbPerDec_4(config.colorHighBoostDbPerDec(), false);
    doSetColorScheme_5(static_cast<int>(config.colorScheme()), false);
    doSetScale_6(static_cast<int>(config.scale()), false);
    doSetAlgorithm_7(static_cast<int>(config.algorithm()), false);
    doSetWindowType_8(static_cast<int>(config.windowType()), false);
    doSetWindowSize_9(1 << config.winSizeLog2(), false);
    doSetZeroPaddingFactor_10(config.zeroPaddingFactor(), false);
}

void TrackSpectrogramSettingsModel::writeToConfig(spectrogram::ISpectrogramConfiguration& config)
{
    config.setSpectralSelectionEnabled(m_spectralSelectionEnabled_1);
    config.setColorGainDb(m_colorGainDb_2);
    config.setColorRangeDb(m_colorRangeDb_3);
    config.setColorHighBoostDbPerDec(m_colorHighBoostDbPerDec_4);
    config.setColorScheme(m_colorScheme_5);
    config.setScale(m_scale_6);
    config.setAlgorithm(m_algorithm_7);
    config.setWindowType(m_windowType_8);
    config.setWinSizeLog2(log2(m_windowSize_9));
    config.setZeroPaddingFactor(m_zeroPaddingFactor_10);
}

void TrackSpectrogramSettingsModel::preview()
{
    const auto trackConfig = Au3TrackSpectrogramConfiguration::create(m_trackId, *globalContext());
    IF_ASSERT_FAILED(trackConfig) {
        return;
    }

    muse::Defer restore{ [trackConfig,
                          spectralSelectionEnabled_1 = trackConfig->spectralSelectionEnabled(),
                          colorGainDb_2 = trackConfig->colorGainDb(),
                          colorRangeDb_3 = trackConfig->colorRangeDb(),
                          colorHighBoostDbPerDec_4 = trackConfig->colorHighBoostDbPerDec(),
                          colorScheme_5 = trackConfig->colorScheme(),
                          scale_6 = trackConfig->scale(),
                          algorithm_7 = trackConfig->algorithm(),
                          windowType_8 = trackConfig->windowType(),
                          winSizeLog2_9 = trackConfig->winSizeLog2(),
                          zeroPaddingFactor_10 = trackConfig->zeroPaddingFactor()
                         ]{
            trackConfig->setSpectralSelectionEnabled(spectralSelectionEnabled_1);
            trackConfig->setColorGainDb(colorGainDb_2);
            trackConfig->setColorRangeDb(colorRangeDb_3);
            trackConfig->setColorHighBoostDbPerDec(colorHighBoostDbPerDec_4);
            trackConfig->setColorScheme(colorScheme_5);
            trackConfig->setScale(scale_6);
            trackConfig->setAlgorithm(algorithm_7);
            trackConfig->setWindowType(windowType_8);
            trackConfig->setWinSizeLog2(winSizeLog2_9);
            trackConfig->setZeroPaddingFactor(zeroPaddingFactor_10);
        } };

    trackConfig->setUseGlobalSettings(m_useGlobalSettings);
    writeToConfig(*trackConfig);

    // Here, something like spectrogramPainter()->paint(m_trackId) - when the spectrogram painting is in place.
}

void TrackSpectrogramSettingsModel::apply()
{
    const auto trackConfig = Au3TrackSpectrogramConfiguration::create(m_trackId, *globalContext());
    IF_ASSERT_FAILED(trackConfig) {
        return;
    }
    trackConfig->setUseGlobalSettings(m_useGlobalSettings);
    writeToConfig(*trackConfig);
}

void TrackSpectrogramSettingsModel::setTrackId(int value)
{
    if (m_trackId == value) {
        return;
    }
    m_trackId = value;
    emit trackIdChanged();
}

QString TrackSpectrogramSettingsModel::trackTitle() const
{
    const ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return QString();
    }
    return QString::fromStdString(project->trackName(m_trackId).value_or(""));
}

bool TrackSpectrogramSettingsModel::useGlobalSettings() const
{
    return m_useGlobalSettings;
}

void TrackSpectrogramSettingsModel::setUseGlobalSettings(bool value)
{
    if (value == useGlobalSettings()) {
        return;
    }
    doSetUseGlobalSettings(value);
    if (value) {
        readFromConfig(*globalSpectrogramConfiguration());
    }
}

void TrackSpectrogramSettingsModel::doSetUseGlobalSettings(bool value)
{
    if (m_useGlobalSettings == value) {
        return;
    }
    m_useGlobalSettings = value;
    emit useGlobalSettingsChanged();
}

bool TrackSpectrogramSettingsModel::spectralSelectionEnabled_1() const
{
    return m_spectralSelectionEnabled_1;
}

void TrackSpectrogramSettingsModel::setSpectralSelectionEnabled_1(bool value)
{
    doSetSpectralSelectionEnabled_1(value, true);
}

void TrackSpectrogramSettingsModel::doSetSpectralSelectionEnabled_1(bool value, bool resetUseGlobalSettings)
{
    if (m_spectralSelectionEnabled_1 == value) {
        return;
    }
    m_spectralSelectionEnabled_1 = value;
    emit spectralSelectionEnabledChanged_1();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::colorGainDb_2() const
{
    return m_colorGainDb_2;
}

void TrackSpectrogramSettingsModel::setColorGainDb_2(int value)
{
    doSetColorGainDb_2(value, true);
}

void TrackSpectrogramSettingsModel::doSetColorGainDb_2(int value, bool resetUseGlobalSettings)
{
    if (value == m_colorGainDb_2) {
        return;
    }
    m_colorGainDb_2 = value;
    emit colorGainDbChanged_2();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::colorRangeDb_3() const
{
    return m_colorRangeDb_3;
}

void TrackSpectrogramSettingsModel::setColorRangeDb_3(int value)
{
    doSetColorRangeDb_3(value, true);
}

void TrackSpectrogramSettingsModel::doSetColorRangeDb_3(int value, bool resetUseGlobalSettings)
{
    if (value == m_colorRangeDb_3) {
        return;
    }
    m_colorRangeDb_3 = value;
    emit colorRangeDbChanged_3();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::colorHighBoostDbPerDec_4() const
{
    return m_colorHighBoostDbPerDec_4;
}

void TrackSpectrogramSettingsModel::setColorHighBoostDbPerDec_4(int value)
{
    doSetColorHighBoostDbPerDec_4(value, true);
}

void TrackSpectrogramSettingsModel::doSetColorHighBoostDbPerDec_4(int value, bool resetUseGlobalSettings)
{
    if (value == m_colorHighBoostDbPerDec_4) {
        return;
    }
    m_colorHighBoostDbPerDec_4 = value;
    emit colorHighBoostDbPerDecChanged_4();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::colorScheme_5() const
{
    return static_cast<int>(m_colorScheme_5);
}

void TrackSpectrogramSettingsModel::setColorScheme_5(int value)
{
    doSetColorScheme_5(value, true);
}

void TrackSpectrogramSettingsModel::doSetColorScheme_5(int value, bool resetUseGlobalSettings)
{
    if (value == static_cast<int>(m_colorScheme_5)) {
        return;
    }
    m_colorScheme_5 = static_cast<spectrogram::SpectrogramColorScheme>(value);
    emit colorSchemeChanged_5();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::scale_6() const
{
    return static_cast<int>(m_scale_6);
}

void TrackSpectrogramSettingsModel::setScale_6(int value)
{
    doSetScale_6(value, true);
}

void TrackSpectrogramSettingsModel::doSetScale_6(int value, bool resetUseGlobalSettings)
{
    if (value == static_cast<int>(m_scale_6)) {
        return;
    }
    m_scale_6 = static_cast<spectrogram::SpectrogramScale>(value);
    emit scaleChanged_6();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::algorithm_7() const
{
    return static_cast<int>(m_algorithm_7);
}

void TrackSpectrogramSettingsModel::setAlgorithm_7(int value)
{
    doSetAlgorithm_7(value, true);
}

void TrackSpectrogramSettingsModel::doSetAlgorithm_7(int value, bool resetUseGlobalSettings)
{
    if (value == static_cast<int>(m_algorithm_7)) {
        return;
    }
    m_algorithm_7 = static_cast<spectrogram::SpectrogramAlgorithm>(value);
    emit algorithmChanged_7();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::windowType_8() const
{
    return static_cast<int>(m_windowType_8);
}

void TrackSpectrogramSettingsModel::setWindowType_8(int value)
{
    doSetWindowType_8(value, true);
}

void TrackSpectrogramSettingsModel::doSetWindowType_8(int value, bool resetUseGlobalSettings)
{
    if (value == static_cast<int>(m_windowType_8)) {
        return;
    }
    m_windowType_8 = static_cast<spectrogram::SpectrogramWindowType>(value);
    emit windowTypeChanged_8();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::windowSize_9() const
{
    return m_windowSize_9;
}

void TrackSpectrogramSettingsModel::setWindowSize_9(int value)
{
    doSetWindowSize_9(value, true);
}

void TrackSpectrogramSettingsModel::doSetWindowSize_9(int value, bool resetUseGlobalSettings)
{
    if (value == m_windowSize_9) {
        return;
    }
    m_windowSize_9 = value;
    emit windowSizeChanged_9();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}

int TrackSpectrogramSettingsModel::zeroPaddingFactor_10() const
{
    return m_zeroPaddingFactor_10;
}

void TrackSpectrogramSettingsModel::setZeroPaddingFactor_10(int value)
{
    doSetZeroPaddingFactor_10(value, true);
}

void TrackSpectrogramSettingsModel::doSetZeroPaddingFactor_10(int value, bool resetUseGlobalSettings)
{
    if (value == m_zeroPaddingFactor_10) {
        return;
    }
    m_zeroPaddingFactor_10 = value;
    emit zeroPaddingFactorChanged_10();
    if (resetUseGlobalSettings) {
        setUseGlobalSettings(false);
    }
}
} // namespace au::spectrogram
