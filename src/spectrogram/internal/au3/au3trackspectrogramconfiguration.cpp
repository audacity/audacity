/*
 * Audacity: A Digital Audio Editor
 */
#include "au3trackspectrogramconfiguration.h"
#include "au3spectrogramutils.h"
#include "au3spectrogramsettings.h"

#include "project/iaudacityproject.h"
#include "au3wrap/internal/domaccessor.h"
#include "context/iglobalcontext.h"

#include "framework/global/log.h"

namespace au::spectrogram {
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
    return std::make_shared<Au3TrackSpectrogramConfiguration>(Au3SpectrogramSettings::Get(*waveTrack));
}

Au3TrackSpectrogramConfiguration::Au3TrackSpectrogramConfiguration(Au3SpectrogramSettings& settings)
    : m_settings(settings)
{}

bool Au3TrackSpectrogramConfiguration::spectralSelectionEnabled() const
{
    return m_settings.spectralSelectionEnabled;
}

void Au3TrackSpectrogramConfiguration::setSpectralSelectionEnabled(bool value)
{
    if (m_settings.spectralSelectionEnabled == value) {
        return;
    }
    m_settings.spectralSelectionEnabled = value;
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
}

SpectrogramColorScheme Au3TrackSpectrogramConfiguration::colorScheme() const
{
    return m_settings.colorScheme;
}

void Au3TrackSpectrogramConfiguration::setColorScheme(SpectrogramColorScheme value)
{
    if (m_settings.colorScheme == value) {
        return;
    }
    m_settings.colorScheme = value;
}

SpectrogramScale Au3TrackSpectrogramConfiguration::scale() const
{
    return m_settings.scaleType;
}

void Au3TrackSpectrogramConfiguration::setScale(SpectrogramScale value)
{
    if (m_settings.scaleType == value) {
        return;
    }
    m_settings.scaleType = value;
}

SpectrogramAlgorithm Au3TrackSpectrogramConfiguration::algorithm() const
{
    return m_settings.algorithm;
}

void Au3TrackSpectrogramConfiguration::setAlgorithm(SpectrogramAlgorithm value)
{
    if (m_settings.algorithm == value) {
        return;
    }
    m_settings.algorithm = value;
}

SpectrogramWindowType Au3TrackSpectrogramConfiguration::windowType() const
{
    return static_cast<SpectrogramWindowType>(fromAu3WindowType(static_cast<::eWindowFunctions>(m_settings.WindowType())));
}

void Au3TrackSpectrogramConfiguration::setWindowType(SpectrogramWindowType value)
{
    if (m_settings.WindowType() == value) {
        return;
    }
    m_settings.SetWindowType(value);
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
}

int Au3TrackSpectrogramConfiguration::zeroPaddingFactor() const
{
    return m_settings.ZeroPaddingFactor();
}

void Au3TrackSpectrogramConfiguration::setZeroPaddingFactor(int value)
{
    if (m_settings.ZeroPaddingFactor() == value) {
        return;
    }
    m_settings.SetZeroPaddingFactor(value);
}

bool Au3TrackSpectrogramConfiguration::useGlobalSettings() const
{
    return m_settings.syncWithGlobalSettings;
}

void Au3TrackSpectrogramConfiguration::setUseGlobalSettings(bool value)
{
    m_settings.syncWithGlobalSettings = value;
}
}
