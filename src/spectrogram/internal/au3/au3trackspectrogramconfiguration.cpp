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

#include "au3-project/Project.h"

namespace au::spectrogram {
std::shared_ptr<Au3TrackSpectrogramConfiguration> Au3TrackSpectrogramConfiguration::create(int trackId,
                                                                                           const context::IGlobalContext& context)
{
    const project::IAudacityProjectPtr project = context.currentProject();
    if (!project) {
        return nullptr;
    }
    auto au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr());
    return std::make_shared<Au3TrackSpectrogramConfiguration>(trackId, *au3Project);
}

Au3TrackSpectrogramConfiguration::Au3TrackSpectrogramConfiguration(int trackId, AudacityProject& project)
    : m_trackId(trackId), m_weakProject(project.shared_from_this())
{
    maybeReloadSettings();
}

bool Au3TrackSpectrogramConfiguration::maybeReloadSettings()
{
    if (!m_weakWaveTrack.expired()) {
        return true;
    }
    const auto project = m_weakProject.lock();
    IF_ASSERT_FAILED(project) {
        return false;
    }
    au3::Au3WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(*project, au3::Au3TrackId { m_trackId });
    if (!waveTrack) {
        return false;
    }
    m_settings = &Au3SpectrogramSettings::Get(*waveTrack);
    return true;
}

double Au3TrackSpectrogramConfiguration::minFreq()
{
    if (!maybeReloadSettings()) {
        return 0.0;
    }
    return m_settings->minFreq;
}

void Au3TrackSpectrogramConfiguration::setMinFreq(double value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    m_settings->minFreq = value;
}

double Au3TrackSpectrogramConfiguration::maxFreq()
{
    if (!maybeReloadSettings()) {
        return 0.0;
    }
    return m_settings->maxFreq;
}

void Au3TrackSpectrogramConfiguration::setMaxFreq(double value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    m_settings->maxFreq = value;
}

int Au3TrackSpectrogramConfiguration::colorGainDb()
{
    if (!maybeReloadSettings()) {
        return 0;
    }
    return m_settings->gain;
}

void Au3TrackSpectrogramConfiguration::setColorGainDb(int value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->gain == value) {
        return;
    }
    m_settings->gain = value;
}

int Au3TrackSpectrogramConfiguration::colorRangeDb()
{
    if (!maybeReloadSettings()) {
        return 0;
    }
    return m_settings->range;
}

void Au3TrackSpectrogramConfiguration::setColorRangeDb(int value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->range == value) {
        return;
    }
    m_settings->range = value;
}

int Au3TrackSpectrogramConfiguration::colorHighBoostDbPerDec()
{
    if (!maybeReloadSettings()) {
        return 0;
    }
    return m_settings->frequencyGain;
}

void Au3TrackSpectrogramConfiguration::setColorHighBoostDbPerDec(int value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->frequencyGain == value) {
        return;
    }
    m_settings->frequencyGain = value;
}

SpectrogramColorScheme Au3TrackSpectrogramConfiguration::colorScheme()
{
    if (!maybeReloadSettings()) {
        return static_cast<SpectrogramColorScheme>(0);
    }
    return m_settings->colorScheme;
}

void Au3TrackSpectrogramConfiguration::setColorScheme(SpectrogramColorScheme value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->colorScheme == value) {
        return;
    }
    m_settings->colorScheme = value;
}

SpectrogramScale Au3TrackSpectrogramConfiguration::scale()
{
    if (!maybeReloadSettings()) {
        return static_cast<SpectrogramScale>(0);
    }
    return m_settings->scaleType;
}

void Au3TrackSpectrogramConfiguration::setScale(SpectrogramScale value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->scaleType == value) {
        return;
    }
    m_settings->scaleType = value;
}

SpectrogramAlgorithm Au3TrackSpectrogramConfiguration::algorithm()
{
    if (!maybeReloadSettings()) {
        return static_cast<SpectrogramAlgorithm>(0);
    }
    return m_settings->algorithm;
}

void Au3TrackSpectrogramConfiguration::setAlgorithm(SpectrogramAlgorithm value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->algorithm == value) {
        return;
    }
    m_settings->algorithm = value;
}

SpectrogramWindowType Au3TrackSpectrogramConfiguration::windowType()
{
    if (!maybeReloadSettings()) {
        return static_cast<SpectrogramWindowType>(0);
    }
    return static_cast<SpectrogramWindowType>(fromAu3WindowType(static_cast<::eWindowFunctions>(m_settings->WindowType())));
}

void Au3TrackSpectrogramConfiguration::setWindowType(SpectrogramWindowType value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->WindowType() == value) {
        return;
    }
    m_settings->SetWindowType(value);
}

int Au3TrackSpectrogramConfiguration::winSizeLog2()
{
    if (!maybeReloadSettings()) {
        return 0;
    }
    return log2(m_settings->WindowSize());
}

void Au3TrackSpectrogramConfiguration::setWinSizeLog2(int logValue)
{
    if (!maybeReloadSettings()) {
        return;
    }
    const int value = 1 << logValue;
    if (m_settings->WindowSize() == value) {
        return;
    }
    m_settings->SetWindowSize(value);
}

int Au3TrackSpectrogramConfiguration::zeroPaddingFactor()
{
    if (!maybeReloadSettings()) {
        return 0;
    }
    return m_settings->ZeroPaddingFactor();
}

void Au3TrackSpectrogramConfiguration::setZeroPaddingFactor(int value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    if (m_settings->ZeroPaddingFactor() == value) {
        return;
    }
    m_settings->SetZeroPaddingFactor(value);
}

bool Au3TrackSpectrogramConfiguration::useGlobalSettings()
{
    if (!maybeReloadSettings()) {
        return false;
    }
    return m_settings->syncWithGlobalSettings;
}

void Au3TrackSpectrogramConfiguration::setUseGlobalSettings(bool value)
{
    if (!maybeReloadSettings()) {
        return;
    }
    m_settings->syncWithGlobalSettings = value;
}
}
