/*
* Audacity: A Digital Audio Editor
*/
#include "au4audioengine.h"

#include <algorithm>
#include <optional>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "au3-project/Project.h"
#include "au3-realtime-effects/RealtimeEffectManager.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-track/Track.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "audio/idevice.h"
#include "iocallback.h"

using namespace au::au4audio;

namespace {
//! How far right of the loop region the playhead may be for loop play to still capture it (as in DefaultPlaybackPolicy)
constexpr double loopCaptureAllowance = 0.5;
}

Au4AudioEngine::Au4AudioEngine(std::shared_ptr<audio::IDeviceOpener> deviceOpener)
    : m_deviceOpener{std::move(deviceOpener)}
{
}

Au4AudioEngine::~Au4AudioEngine() = default;

void Au4AudioEngine::setDriverController(const audio::IAudioDriverController* driverController)
{
    m_driverController = driverController;
}

bool Au4AudioEngine::isBusy() const
{
    return m_stream != nullptr;
}

bool Au4AudioEngine::isCapturing() const
{
    return false;
}

bool Au4AudioEngine::isMonitoring() const
{
    return false;
}

std::optional<au::audio::AudioStreamDescriptor> Au4AudioEngine::currentStream() const
{
    if (!m_stream) {
        return std::nullopt;
    }
    return audio::AudioStreamDescriptor { audio::AudioStreamKind::Playback, &m_stream->project, m_stream->sampleRate() };
}

bool Au4AudioEngine::isStreamActive(AudacityProject& project) const
{
    return m_stream && &m_stream->project == &project && !m_stream->isFinished();
}

double Au4AudioEngine::transportTime() const
{
    return m_transportTime;
}

int Au4AudioEngine::startTransport(const TransportSequences& sequences, double startTime, double endTime,
                                   double /*mixerEndTime*/, AudacityProject& project, const StartTransportOptions& options)
{
    if (m_stream) {
        return 0;
    }
    if (!sequences.captureSequences.empty()) {
        m_lastError = u"Recording with this audio engine is not implemented yet";
        return 0;
    }
    if (sequences.playbackSequences.empty()) {
        m_lastError = u"Nothing to play";
        return 0;
    }
    IF_ASSERT_FAILED(m_driverController) {
        return 0;
    }
    auto device = openDevice(0, numChannels, options.sampleRate);
    if (!device) {
        return 0;
    }

    // As AudioIO: the default policy plays from the play region start, and loops it if it is active
    const auto& playRegion = ViewInfo::Get(project).playRegion;
    std::optional<double> streamStartTime = options.streamStartTime;
    if (!streamStartTime && options.isDefaultPolicy) {
        streamStartTime = std::max(0.0, playRegion.GetStart());
    }
    const double playStartTime = streamStartTime.value_or(startTime);

    // In ms in the settings
    const double softwareBufferLength = m_driverController->configuration().softwareBufferLength / 1000.0;
    auto stream = std::make_unique<IoCallback>(project, m_nextStreamToken, std::move(device), options.isDefaultPolicy,
                                               m_callbackInfoQueue, m_playedRanges, m_playbackVolume, softwareBufferLength);
    stream->trackEndTime = TrackList::Get(project).GetEndTime();
    stream->build(sequences, muse::value(m_playbackMeters, &project));

    std::optional<std::pair<double, double> > loopRange;
    if (options.isDefaultPolicy && playRegion.Active() && startTime < endTime
        && playStartTime <= endTime + loopCaptureAllowance) {
        loopRange.emplace(startTime, endTime);
    }
    stream->setPlayRange(playStartTime, endTime, loopRange);

    m_callbackInfoQueue.Clear();
    m_playedRanges.Clear();
    m_currentPlayedRange = { playStartTime, 0 };
    m_consumedInCurrentRange = 0;
    m_transportTime = playStartTime;
    m_lastSampleRate = stream->sampleRate();

    m_stream = std::move(stream);
    if (m_stream->followsPlayRegion) {
        m_playRegionSubscription = ViewInfo::Get(project).playRegion.Subscribe([this](auto&) { onPlayRegionChanged(); });
    }
    m_stream->start();

    LOGI() << m_deviceOpener->name() << " engine playing at " << m_lastSampleRate << " Hz";
    return m_nextStreamToken++;
}

void Au4AudioEngine::onPlayRegionChanged()
{
    if (!m_stream) {
        return;
    }
    const auto& playRegion = ViewInfo::Get(m_stream->project).playRegion;
    const double t0 = playRegion.GetStart();
    const double t1 = playRegion.GetEnd();
    std::optional<std::pair<double, double> > loopRange;
    if (playRegion.Active() && t0 < t1 && m_stream->position() <= t1 + loopCaptureAllowance) {
        loopRange.emplace(t0, t1);
    }
    // Without a loop, play on to the end of the tracks
    m_stream->updateLoop(loopRange, std::max(t0, m_stream->trackEndTime));
}

void Au4AudioEngine::stopTransport()
{
    if (!m_stream) {
        return;
    }
    m_playRegionSubscription.Reset();
    m_stream->stop();
    m_stream.reset();
    m_streamStopped.notify();
}

void Au4AudioEngine::pauseTransport(bool pause)
{
    if (m_stream) {
        m_stream->setPaused(pause);
    }
}

void Au4AudioEngine::seek(double time)
{
    if (m_stream) {
        m_stream->seek(time);
    }
}

std::shared_ptr<RealtimeEffectState> Au4AudioEngine::addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                            const std::string& effectId)
{
    return RealtimeEffectManager::Get(project).AddState(effectsInitialization(project), group, au3::wxFromStdString(effectId));
}

void Au4AudioEngine::removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                               const std::shared_ptr<RealtimeEffectState>& state)
{
    RealtimeEffectManager::Get(project).RemoveState(effectsInitialization(project), group, state);
}

std::shared_ptr<RealtimeEffectState> Au4AudioEngine::replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                                size_t effectListIndex,
                                                                                const std::string& newEffectId)
{
    return RealtimeEffectManager::Get(project).ReplaceState(effectsInitialization(project), group, effectListIndex,
                                                            au3::wxFromStdString(newEffectId));
}

RealtimeEffects::InitializationScope* Au4AudioEngine::effectsInitialization(AudacityProject& project) const
{
    return m_stream && &m_stream->project == &project ? m_stream->effectsInitialization() : nullptr;
}

void Au4AudioEngine::setPlaybackMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter)
{
    m_playbackMeters[&project] = meter;
}

void Au4AudioEngine::setCaptureMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter)
{
    m_captureMeters[&project] = meter;
}

void Au4AudioEngine::stopMeters(AudacityProject& project)
{
    for (const auto* meters : { &m_playbackMeters, &m_captureMeters }) {
        if (const auto meter = muse::value(*meters, &project)) {
            meter->stop();
        }
    }
}

void Au4AudioEngine::startMonitoring(AudacityProject&)
{
}

void Au4AudioEngine::stopMonitoring()
{
}

std::unique_ptr<au::audio::IDevice> Au4AudioEngine::openDevice(int numInputChannels, int numOutputChannels, double sampleRate)
{
    IF_ASSERT_FAILED(m_driverController) {
        return nullptr;
    }
    std::string error;
    auto device = m_deviceOpener->open(m_driverController->configuration(), numInputChannels, numOutputChannels, sampleRate, error);
    if (!device) {
        m_lastError = muse::String::fromStdString(error);
        return nullptr;
    }
    return device;
}

void Au4AudioEngine::setInputVolume(float newInputVolume)
{
    // Applied to the input, as AudioIO does where it can't set the volume of the device
    m_inputVolume = newInputVolume;
}

float Au4AudioEngine::getInputVolume() const
{
    return m_inputVolume;
}

void Au4AudioEngine::setPlaybackVolume(float newPlaybackVolume)
{
    m_playbackVolume = newPlaybackVolume;
}

float Au4AudioEngine::getPlaybackVolume() const
{
    return m_playbackVolume;
}

bool Au4AudioEngine::canStopTransport(AudacityProject& project) const
{
    return !m_stream || &m_stream->project == &project;
}

void Au4AudioEngine::handleDeviceChange()
{
    // The device is opened for each stream, and the driver controller suspends streams before changing it
}

muse::String Au4AudioEngine::lastErrorString() const
{
    return m_lastError;
}

double Au4AudioEngine::getPlaybackSampleRate() const
{
    return m_lastSampleRate;
}

void Au4AudioEngine::updateTimePosition(unsigned long newlyConsumedSamples)
{
    while (newlyConsumedSamples > 0) {
        if (m_consumedInCurrentRange == m_currentPlayedRange.frames) {
            PlayedRange next;
            if (!m_playedRanges.Get(next)) {
                break;
            }
            m_currentPlayedRange = next;
            m_consumedInCurrentRange = 0;
        }
        const auto consumed = std::min(newlyConsumedSamples, m_currentPlayedRange.frames - m_consumedInCurrentRange);
        m_consumedInCurrentRange += consumed;
        newlyConsumedSamples -= consumed;
    }
    if (m_lastSampleRate > 0) {
        m_transportTime = m_currentPlayedRange.time + m_consumedInCurrentRange / m_lastSampleRate;
    }
}

std::optional<au::audio::AudioCallbackInfo> Au4AudioEngine::consumeNextCallbackInfo()
{
    audio::AudioCallbackInfo info;
    if (m_callbackInfoQueue.Get(info)) {
        return info;
    }
    return std::nullopt;
}

muse::async::Notification Au4AudioEngine::recordingUpdateRequested() const
{
    return m_listener->recordingUpdateRequested();
}

muse::async::Notification Au4AudioEngine::recordingNewBlocks() const
{
    return m_listener->recordingNewBlocks();
}

muse::async::Notification Au4AudioEngine::recordingCommitRequested() const
{
    return m_listener->recordingCommitRequested();
}

muse::async::Notification Au4AudioEngine::recordingFinished() const
{
    return m_listener->recordingFinished();
}

muse::async::Notification Au4AudioEngine::streamStopped() const
{
    return m_streamStopped;
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> Au4AudioEngine::recordingClipChanged() const
{
    return m_listener->recordingClipChanged();
}
