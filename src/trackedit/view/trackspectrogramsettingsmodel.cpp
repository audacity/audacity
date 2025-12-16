/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramsettingsmodel.h"
#include "internal/snapshotspectrogramconfiguration.h"
#include "spectrogram/itrackspectrogramconfigurationprovider.h"

#include "framework/global/log.h"

namespace au::trackedit {
namespace {
constexpr auto isPowerOfTwo(int x) -> bool
{
    return (x != 0) && ((x & (x - 1)) == 0);
}

static_assert(isPowerOfTwo(3) == false);
static_assert(isPowerOfTwo(4) == true);

constexpr auto logTwo(int x)
{
    assert(isPowerOfTwo(x));
    auto result = 0;
    while (x > 1) {
        x >>= 1;
        ++result;
    }
    return result;
}

static_assert(logTwo(1) == 0);
static_assert(logTwo(2) == 1);
static_assert(logTwo(4) == 2);
static_assert(logTwo(8) == 3);
}

TrackSpectrogramSettingsModel::TrackSpectrogramSettingsModel(QObject* parent)
    : spectrogram::AbstractSpectrogramSettingsModel(parent)
{}

TrackSpectrogramSettingsModel::~TrackSpectrogramSettingsModel()
{
    if (m_initialTrackConfig) {
        trackSpectrogramConfigurationProvider()->copyConfiguration(*m_initialTrackConfig, *m_trackConfig);
        m_trackConfig->setUseGlobalSettings(m_initialTrackConfig->useGlobalSettings());
        sendRepaintRequest();
    }
}

void TrackSpectrogramSettingsModel::componentComplete()
{
    m_trackConfig = trackSpectrogramConfigurationProvider()->trackSpectrogramConfiguration(m_trackId);
    IF_ASSERT_FAILED(m_trackConfig) {
        return;
    }

    m_initialTrackConfig = std::make_unique<SnapshotSpectrogramConfiguration>(*m_trackConfig);

    emit spectralSelectionEnabledChanged_1();
    emit colorGainDbChanged_2();
    emit colorRangeDbChanged_3();
    emit colorHighBoostDbPerDecChanged_4();
    emit colorSchemeChanged_5();
    emit scaleChanged_6();
    emit algorithmChanged_7();
    emit windowTypeChanged_8();
    emit windowSizeChanged_9();
    emit zeroPaddingFactorChanged_10();
    emit useGlobalSettingsChanged();

    sendRepaintRequest();
}

void TrackSpectrogramSettingsModel::onSettingChanged()
{
    setUseGlobalSettings(false);
    sendRepaintRequest();
}

void TrackSpectrogramSettingsModel::sendRepaintRequest()
{
    const ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project) {
        return;
    }
    const auto track = project->track(m_trackId);
    IF_ASSERT_FAILED(track) {
        return;
    }
    project->notifyAboutTrackChanged(*track);
}

void TrackSpectrogramSettingsModel::accept()
{
    m_initialTrackConfig.reset();
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
    return m_trackConfig ? m_trackConfig->useGlobalSettings() : false;
}

void TrackSpectrogramSettingsModel::setUseGlobalSettings(bool value)
{
    IF_ASSERT_FAILED(m_trackConfig) {
        return;
    }
    if (m_trackConfig->useGlobalSettings() == value) {
        return;
    }
    m_trackConfig->setUseGlobalSettings(value);
    if (value) {
        trackSpectrogramConfigurationProvider()->copyConfiguration(*globalSpectrogramConfiguration(), *m_trackConfig);
        emit spectralSelectionEnabledChanged_1();
        emit colorGainDbChanged_2();
        emit colorRangeDbChanged_3();
        emit colorHighBoostDbPerDecChanged_4();
        emit colorSchemeChanged_5();
        emit scaleChanged_6();
        emit algorithmChanged_7();
        emit windowTypeChanged_8();
        emit windowSizeChanged_9();
        emit zeroPaddingFactorChanged_10();
        sendRepaintRequest();
    }
    emit useGlobalSettingsChanged();
}

bool TrackSpectrogramSettingsModel::spectralSelectionEnabled_1() const
{
    return m_trackConfig ? m_trackConfig->spectralSelectionEnabled() : false;
}

void TrackSpectrogramSettingsModel::setSpectralSelectionEnabled_1(bool value)
{
    if (m_trackConfig->spectralSelectionEnabled() == value) {
        return;
    }
    m_trackConfig->setSpectralSelectionEnabled(value);
    emit spectralSelectionEnabledChanged_1();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::colorGainDb_2() const
{
    return m_trackConfig ? m_trackConfig->colorGainDb() : 0;
}

void TrackSpectrogramSettingsModel::setColorGainDb_2(int value)
{
    if (m_trackConfig->colorGainDb() == value) {
        return;
    }
    m_trackConfig->setColorGainDb(value);
    emit colorGainDbChanged_2();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::colorRangeDb_3() const
{
    return m_trackConfig ? m_trackConfig->colorRangeDb() : 0;
}

void TrackSpectrogramSettingsModel::setColorRangeDb_3(int value)
{
    if (m_trackConfig->colorRangeDb() == value) {
        return;
    }
    m_trackConfig->setColorRangeDb(value);
    emit colorRangeDbChanged_3();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::colorHighBoostDbPerDec_4() const
{
    return m_trackConfig ? m_trackConfig->colorHighBoostDbPerDec() : 0;
}

void TrackSpectrogramSettingsModel::setColorHighBoostDbPerDec_4(int value)
{
    if (m_trackConfig->colorHighBoostDbPerDec() == value) {
        return;
    }
    m_trackConfig->setColorHighBoostDbPerDec(value);
    emit colorHighBoostDbPerDecChanged_4();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::colorScheme_5() const
{
    return m_trackConfig ? static_cast<int>(m_trackConfig->colorScheme()) : 0;
}

void TrackSpectrogramSettingsModel::setColorScheme_5(int value)
{
    const auto scheme = static_cast<spectrogram::SpectrogramColorScheme>(value);
    if (m_trackConfig->colorScheme() == scheme) {
        return;
    }
    m_trackConfig->setColorScheme(scheme);
    emit colorSchemeChanged_5();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::scale_6() const
{
    return m_trackConfig ? static_cast<int>(m_trackConfig->scale()) : 0;
}

void TrackSpectrogramSettingsModel::setScale_6(int value)
{
    const auto scale = static_cast<spectrogram::SpectrogramScale>(value);
    if (m_trackConfig->scale() == scale) {
        return;
    }
    m_trackConfig->setScale(scale);
    emit scaleChanged_6();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::algorithm_7() const
{
    return m_trackConfig ? static_cast<int>(m_trackConfig->algorithm()) : 0;
}

void TrackSpectrogramSettingsModel::setAlgorithm_7(int value)
{
    const auto algorithm = static_cast<spectrogram::SpectrogramAlgorithm>(value);
    if (m_trackConfig->algorithm() == algorithm) {
        return;
    }
    m_trackConfig->setAlgorithm(algorithm);
    emit algorithmChanged_7();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::windowType_8() const
{
    return m_trackConfig ? static_cast<int>(m_trackConfig->windowType()) : 0;
}

void TrackSpectrogramSettingsModel::setWindowType_8(int value)
{
    const auto windowType = static_cast<spectrogram::SpectrogramWindowType>(value);
    if (m_trackConfig->windowType() == windowType) {
        return;
    }
    m_trackConfig->setWindowType(windowType);
    emit windowTypeChanged_8();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::windowSize_9() const
{
    return m_trackConfig ? 1 << m_trackConfig->winSizeLog2() : 0;
}

void TrackSpectrogramSettingsModel::setWindowSize_9(int value)
{
    assert(isPowerOfTwo(value));
    if (m_trackConfig->winSizeLog2() == logTwo(value)) {
        return;
    }
    m_trackConfig->setWinSizeLog2(logTwo(value));
    emit windowSizeChanged_9();
    onSettingChanged();
}

int TrackSpectrogramSettingsModel::zeroPaddingFactor_10() const
{
    return m_trackConfig ? m_trackConfig->zeroPaddingFactor() : 0;
}

void TrackSpectrogramSettingsModel::setZeroPaddingFactor_10(int value)
{
    if (m_trackConfig->zeroPaddingFactor() == value) {
        return;
    }
    m_trackConfig->setZeroPaddingFactor(value);
    emit zeroPaddingFactorChanged_10();
    onSettingChanged();
}
} // namespace au::spectrogram
