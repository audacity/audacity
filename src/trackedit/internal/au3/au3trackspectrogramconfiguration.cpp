/*
 * Audacity: A Digital Audio Editor
 */
#include "au3trackspectrogramconfiguration.h"
#include "au3trackspectrogramutils.h"

#include "project/iaudacityproject.h"
#include "au3wrap/internal/domaccessor.h"
#include "context/iglobalcontext.h"

#include "framework/global/log.h"

#include "spectrogram/internal/au3/SpectrogramSettings.h" // for now, track settings provider coming

namespace au::trackedit {
std::shared_ptr<Au3TrackSpectrogramConfiguration> Au3TrackSpectrogramConfiguration::create(int trackId,
                                                                                           const au::context::IGlobalContext& context)
{
    const project::IAudacityProjectPtr project = context.currentProject();
    if (!project) {
        return nullptr;
    }
    auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    au::au3::Au3WaveTrack* waveTrack = au::au3::DomAccessor::findWaveTrack(*au3Project, au::au3::Au3TrackId { trackId });
    IF_ASSERT_FAILED(waveTrack) {
        return nullptr;
    }
    return std::make_shared<Au3TrackSpectrogramConfiguration>(spectrogram::SpectrogramSettings::Get(*waveTrack));
}

Au3TrackSpectrogramConfiguration::Au3TrackSpectrogramConfiguration(spectrogram::SpectrogramSettings& settings)
    : m_settings(settings)
{}

bool Au3TrackSpectrogramConfiguration::spectralSelectionEnabled() const
{
    return m_settings.spectralSelection;
}

void Au3TrackSpectrogramConfiguration::setSpectralSelectionEnabled(bool value)
{
    if (m_settings.spectralSelection == value) {
        return;
    }
    m_settings.spectralSelection = value;
    m_spectralSelectionEnabledChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<bool> Au3TrackSpectrogramConfiguration::spectralSelectionEnabledChanged() const
{
    return m_spectralSelectionEnabledChanged;
}

muse::async::Channel<int> Au3TrackSpectrogramConfiguration::colorGainDbChanged() const
{
    return m_colorGainDbChanged;
}

int Au3TrackSpectrogramConfiguration::colorGainDb() const
{
    return m_settings.gain;
}

void Au3TrackSpectrogramConfiguration::setColorGainDb(int value)
{
    if (m_settings.gain == value) {
        return;
    }
    m_settings.gain = value;
    m_colorGainDbChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<int> Au3TrackSpectrogramConfiguration::colorRangeDbChanged() const
{
    return m_colorRangeDbChanged;
}

int Au3TrackSpectrogramConfiguration::colorRangeDb() const
{
    return m_settings.range;
}

void Au3TrackSpectrogramConfiguration::setColorRangeDb(int value)
{
    if (m_settings.range == value) {
        return;
    }
    m_settings.range = value;
    m_colorRangeDbChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<int> Au3TrackSpectrogramConfiguration::colorHighBoostDbPerDecChanged() const
{
    return m_colorHighBoostDbPerDecChanged;
}

int Au3TrackSpectrogramConfiguration::colorHighBoostDbPerDec() const
{
    return m_settings.frequencyGain;
}

void Au3TrackSpectrogramConfiguration::setColorHighBoostDbPerDec(int value)
{
    if (m_settings.frequencyGain == value) {
        return;
    }
    m_settings.frequencyGain = value;
    m_colorHighBoostDbPerDecChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<spectrogram::SpectrogramColorScheme> Au3TrackSpectrogramConfiguration::colorSchemeChanged() const
{
    return m_colorSchemeChanged;
}

spectrogram::SpectrogramColorScheme Au3TrackSpectrogramConfiguration::colorScheme() const
{
    return static_cast<spectrogram::SpectrogramColorScheme>(fromAu3ColorScheme(m_settings.colorScheme));
}

void Au3TrackSpectrogramConfiguration::setColorScheme(spectrogram::SpectrogramColorScheme value)
{
    const auto au3value = toAu3ColorScheme(value);
    if (m_settings.colorScheme == au3value) {
        return;
    }
    m_settings.colorScheme = au3value;
    m_colorSchemeChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<spectrogram::SpectrogramScale> Au3TrackSpectrogramConfiguration::scaleChanged() const
{
    return m_scaleChanged;
}

spectrogram::SpectrogramScale Au3TrackSpectrogramConfiguration::scale() const
{
    return static_cast<spectrogram::SpectrogramScale>(fromAu3Scale(static_cast<spectrogram::SpectrogramSettings::ScaleTypeValues>(m_settings
                                                                                                                                  .scaleType)));
}

void Au3TrackSpectrogramConfiguration::setScale(spectrogram::SpectrogramScale value)
{
    const auto au3value = toAu3Scale(value);
    if (m_settings.scaleType == au3value) {
        return;
    }
    m_settings.scaleType = au3value;
    m_scaleChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<spectrogram::SpectrogramAlgorithm> Au3TrackSpectrogramConfiguration::algorithmChanged() const
{
    return m_algorithmChanged;
}

spectrogram::SpectrogramAlgorithm Au3TrackSpectrogramConfiguration::algorithm() const
{
    return static_cast<spectrogram::SpectrogramAlgorithm>(fromAu3Algorithm(static_cast<spectrogram::SpectrogramSettings::AlgorithmValues>(
                                                                               m_settings.algorithm)));
}

void Au3TrackSpectrogramConfiguration::setAlgorithm(spectrogram::SpectrogramAlgorithm value)
{
    const auto au3value = toAu3Algorithm(value);
    if (m_settings.algorithm == au3value) {
        return;
    }
    m_settings.algorithm = au3value;
    m_algorithmChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<spectrogram::SpectrogramWindowType> Au3TrackSpectrogramConfiguration::windowTypeChanged() const
{
    return m_windowTypeChanged;
}

spectrogram::SpectrogramWindowType Au3TrackSpectrogramConfiguration::windowType() const
{
    return static_cast<spectrogram::SpectrogramWindowType>(fromAu3WindowType(static_cast<::eWindowFunctions>(m_settings.windowType)));
}

void Au3TrackSpectrogramConfiguration::setWindowType(spectrogram::SpectrogramWindowType value)
{
    const auto au3value = toAu3WindowType(value);
    if (m_settings.windowType == au3value) {
        return;
    }
    m_settings.windowType = au3value;
    m_windowTypeChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<int> Au3TrackSpectrogramConfiguration::winSizeLog2Changed() const
{
    return m_winSizeLog2Changed;
}

int Au3TrackSpectrogramConfiguration::winSizeLog2() const
{
    return log2(m_settings.WindowSize());
}

void Au3TrackSpectrogramConfiguration::setWinSizeLog2(int logValue)
{
    const int value = 1 << logValue;
    if (m_settings.WindowSize() == value) {
        return;
    }
    m_settings.SetWindowSize(value);
    m_winSizeLog2Changed.send(logValue);
    m_someSettingChanged.notify();
}

int Au3TrackSpectrogramConfiguration::zeroPaddingFactor() const
{
    return m_settings.ZeroPaddingFactor();
}

void Au3TrackSpectrogramConfiguration::setZeroPaddingFactor(int value)
{
    if (m_settings.zeroPaddingFactor == value) {
        return;
    }
    m_settings.zeroPaddingFactor = value;
    m_zeroPaddingFactorChanged.send(value);
    m_someSettingChanged.notify();
}

muse::async::Channel<int> Au3TrackSpectrogramConfiguration::zeroPaddingFactorChanged() const
{
    return m_zeroPaddingFactorChanged;
}

bool Au3TrackSpectrogramConfiguration::useGlobalSettings() const
{
    return m_settings.syncWithGlobalSettings;
}

void Au3TrackSpectrogramConfiguration::setUseGlobalSettings(bool value)
{
    m_settings.syncWithGlobalSettings = value;
}

spectrogram::AllSpectrogramSettings Au3TrackSpectrogramConfiguration::allSettings() const
{
    spectrogram::AllSpectrogramSettings allSettings;
    allSettings.spectralSelectionEnabled = spectralSelectionEnabled();
    allSettings.colorGainDb = colorGainDb();
    allSettings.colorRangeDb = colorRangeDb();
    allSettings.colorHighBoostDbPerDec = colorHighBoostDbPerDec();
    allSettings.colorScheme = colorScheme();
    allSettings.scale = scale();
    allSettings.algorithm = algorithm();
    allSettings.windowType = windowType();
    allSettings.winSizeLog2 = winSizeLog2();
    allSettings.zeroPaddingFactor = zeroPaddingFactor();
    return allSettings;
}

void Au3TrackSpectrogramConfiguration::setAllSettings(const spectrogram::AllSpectrogramSettings& allSettings)
{
    setSpectralSelectionEnabled(allSettings.spectralSelectionEnabled);
    setColorGainDb(allSettings.colorGainDb);
    setColorRangeDb(allSettings.colorRangeDb);
    setColorHighBoostDbPerDec(allSettings.colorHighBoostDbPerDec);
    setColorScheme(allSettings.colorScheme);
    setScale(allSettings.scale);
    setAlgorithm(allSettings.algorithm);
    setWindowType(allSettings.windowType);
    setWinSizeLog2(allSettings.winSizeLog2);
    setZeroPaddingFactor(allSettings.zeroPaddingFactor);
}

muse::async::Notification Au3TrackSpectrogramConfiguration::someSettingChanged() const
{
    return m_someSettingChanged;
}
} // namespace au::spectrogram
