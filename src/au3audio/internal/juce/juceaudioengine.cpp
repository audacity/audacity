/*
* Audacity: A Digital Audio Editor
*/
#include "juceaudioengine.h"

#include <algorithm>
#include <optional>

#include "framework/global/containers.h"
#include "framework/global/log.h"

#include "au3-preferences/Prefs.h"
#include "au3-project/Project.h"
#include "au3-project-rate/ProjectRate.h"
#include "au3-realtime-effects/RealtimeEffectManager.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-track/Track.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "fastiocallback.h"
#include "juceaudiodevices.h"
#include "jucemonitoringcallback.h"
#include "recordingcapture.h"
#include "safeiocallback.h"

using namespace au::au4juce;

namespace {
//! Where a stream that plays or records until stopped ends
constexpr double maxStreamTime = 1e7;
//! How far right of the loop region the playhead may be for loop play to still capture it (as in DefaultPlaybackPolicy)
constexpr double loopCaptureAllowance = 0.5;

std::string effectiveOutputDevice(const JuceAudioDevices& devices, const au::audio::AudioConfiguration& configuration)
{
    if (configuration.outputDevice && muse::contains(devices.outputDevices(configuration.api), *configuration.outputDevice)) {
        return *configuration.outputDevice;
    }
    return devices.systemDefaultOutputDevice(configuration.api);
}

std::string effectiveInputDevice(const JuceAudioDevices& devices, const au::audio::AudioConfiguration& configuration)
{
    if (configuration.inputDevice && muse::contains(devices.inputDevices(configuration.api), *configuration.inputDevice)) {
        return *configuration.inputDevice;
    }
    return devices.systemDefaultInputDevice(configuration.api);
}

//! The requested rate if the device supports it, else its highest
double bestSampleRate(juce::AudioIODevice& device, double requested)
{
    const auto rates = device.getAvailableSampleRates();
    if (rates.isEmpty() || rates.contains(requested)) {
        return requested;
    }
    return *std::max_element(rates.begin(), rates.end());
}

//! As AudioIO asks PortAudio for `bufferLength` ms of latency: the longest buffer of the device that isn't longer, or its
//! shortest. JUCE's ALSA devices list sizes without asking the device: with PipeWire, the shortest (32 frames) may open but
//! stay silent.
int bufferSize(juce::AudioIODevice& device, double bufferLength, double sampleRate)
{
    auto sizes = device.getAvailableBufferSizes();
    if (sizes.isEmpty()) {
        return device.getDefaultBufferSize();
    }
    std::sort(sizes.begin(), sizes.end());
    const auto wanted = bufferLength / 1000.0 * sampleRate;
    int size = sizes.getFirst();
    for (const auto candidate : sizes) {
        if (candidate <= wanted) {
            size = candidate;
        }
    }
    return size;
}
}

JuceAudioEngine::JuceAudioEngine(std::shared_ptr<JuceAudioDevices> devices)
    : m_devices{std::move(devices)}
{
}

JuceAudioEngine::~JuceAudioEngine() = default;

void JuceAudioEngine::setMode(Mode mode)
{
    m_mode = mode;
}

void JuceAudioEngine::setDriverController(const audio::IAudioDriverController* driverController)
{
    m_driverController = driverController;
}

bool JuceAudioEngine::isBusy() const
{
    return m_stream != nullptr;
}

bool JuceAudioEngine::isCapturing() const
{
    // As AudioIO, once past the lead-in
    return m_stream && !m_stream->isFinished() && m_stream->capture() && m_stream->capture()->isCapturing(m_stream->position());
}

bool JuceAudioEngine::isMonitoring() const
{
    return m_monitoring != nullptr;
}

std::optional<au::audio::AudioStreamDescriptor> JuceAudioEngine::currentStream() const
{
    if (m_monitoring) {
        return audio::AudioStreamDescriptor { audio::AudioStreamKind::Monitoring, &m_monitoring->project, m_monitoring->sampleRate() };
    }
    if (!m_stream) {
        return std::nullopt;
    }
    const auto kind = m_stream->capture() ? audio::AudioStreamKind::Recording : audio::AudioStreamKind::Playback;
    return audio::AudioStreamDescriptor { kind, &m_stream->project, m_stream->sampleRate() };
}

bool JuceAudioEngine::isStreamActive(AudacityProject& project) const
{
    return m_stream && &m_stream->project == &project && !m_stream->isFinished();
}

double JuceAudioEngine::transportTime() const
{
    return m_transportTime;
}

int JuceAudioEngine::startTransport(const TransportSequences& sequences, double startTime, double endTime,
                                    double /*mixerEndTime*/, AudacityProject& project, const StartTransportOptions& options)
{
    if (m_stream) {
        return 0;
    }
    const bool recording = !sequences.captureSequences.empty();
    if (sequences.playbackSequences.empty() && !recording) {
        m_lastError = u"Nothing to play";
        return 0;
    }
    // The graphs are read rather than the sequences
    IF_ASSERT_FAILED(sequences.playbackGraphs.size() == sequences.playbackSequences.size()
                     && std::all_of(sequences.playbackGraphs.begin(), sequences.playbackGraphs.end(),
                                    [](const auto& graph) { return graph != nullptr; })) {
        return 0;
    }
    IF_ASSERT_FAILED(m_driverController) {
        return 0;
    }
    // As AudioIO, playing stops monitoring
    stopMonitoring();
    const auto configuration = m_driverController->configuration();
    auto device = openDevice(recording ? configuration.inputChannels : 0, numChannels, options.sampleRate);
    if (!device) {
        return 0;
    }

    // As AudioIO, recording plays from before where it starts by the lead-in, and to after where it ends by the latency
    // it compensates
    std::optional<RecordingCapture::Settings> capture;
    double rangeStart = startTime;
    double rangeEnd = endTime;
    if (recording) {
        RecordingCapture::Settings settings;
        settings.sequences = sequences.captureSequences;
        settings.startTime = startTime;
        settings.endTime = endTime;
        settings.leadInTime = std::max(0.0, std::min(startTime, options.leadInTime));
        const auto rate = device->getCurrentSampleRate();
        settings.latencyCompensation = configuration.automaticLatencyCompensation
                                       ? -(device->getInputLatencyInSamples() + device->getOutputLatencyInSamples()) / rate
                                       : configuration.latencyCompensation / 1000.0;
        settings.crossfadeData = options.crossfadeData;
        settings.detectDropouts = gPrefs->Read(WarningDialogKey(wxT("DropoutDetected")), true) != 0;
        settings.numInputChannels = device->getActiveInputChannels().countNumberOfSetBits();
        settings.deviceRate = rate;
        settings.maxFrames = std::max(1, device->getCurrentBufferSizeSamples());
        if (settings.numInputChannels == 0) {
            m_lastError = u"The input device has no channels";
            return 0;
        }
        rangeStart = startTime - settings.leadInTime;
        // Recording until stopped ends very far away
        rangeEnd = std::min(endTime - settings.latencyCompensation, maxStreamTime);
        capture = std::move(settings);
    }

    // As AudioIO: the default policy plays from the play region start, and loops it if it is active
    const auto& playRegion = ViewInfo::Get(project).playRegion;
    std::optional<double> streamStartTime = options.streamStartTime;
    if (!streamStartTime && options.isDefaultPolicy) {
        streamStartTime = std::max(0.0, playRegion.GetStart());
    }
    const double playStartTime = streamStartTime.value_or(rangeStart);

    std::unique_ptr<JuceIoCallback> stream;
    if (m_mode == Mode::Fast) {
        stream = std::make_unique<FastIoCallback>(project, m_nextStreamToken, std::move(device), options.isDefaultPolicy,
                                                  m_callbackInfoQueue, m_playedRanges, m_playbackVolume);
    } else {
        stream = std::make_unique<SafeIoCallback>(project, m_nextStreamToken, std::move(device), options.isDefaultPolicy,
                                                  m_callbackInfoQueue, m_playedRanges, m_playbackVolume);
    }
    stream->trackEndTime = TrackList::Get(project).GetEndTime();
    std::optional<JuceIoCallback::RecordingRange> recordingRange;
    if (recording) {
        recordingRange = JuceIoCallback::RecordingRange { startTime, endTime };
    }
    stream->build(sequences, muse::value(m_playbackMeters, &project), recordingRange);
    if (capture) {
        auto recordingCapture = std::make_unique<RecordingCapture>(project, std::move(*capture), m_listener,
                                                                   muse::value(m_captureMeters, &project), m_inputVolume,
                                                                   recordConfiguration()->isInputMonitoringOn(),
                                                                   [this] { stopTransport(); });
        if (!recordingCapture->start()) {
            m_lastError = u"Cannot allocate the recording buffers";
            return 0;
        }
        stream->setCapture(std::move(recordingCapture));
    }

    std::optional<std::pair<double, double> > loopRange;
    if (options.isDefaultPolicy && playRegion.Active() && startTime < endTime
        && playStartTime <= endTime + loopCaptureAllowance) {
        loopRange.emplace(startTime, endTime);
    }
    stream->setPlayRange(playStartTime, rangeEnd, loopRange);

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

    LOGI() << "JUCE engine " << (recording ? "recording" : "playing") << " at " << m_lastSampleRate << " Hz, "
           << (m_mode == Mode::Fast ? "fast" : "safe");
    return m_nextStreamToken++;
}

void JuceAudioEngine::onPlayRegionChanged()
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

void JuceAudioEngine::stopTransport()
{
    if (!m_stream) {
        return;
    }
    m_playRegionSubscription.Reset();
    m_stream->stop();
    m_stream.reset();
    m_streamStopped.notify();
}

void JuceAudioEngine::pauseTransport(bool pause)
{
    if (m_stream) {
        m_stream->setPaused(pause);
    }
}

void JuceAudioEngine::seek(double time)
{
    if (m_stream) {
        m_stream->seek(time);
    }
}

std::shared_ptr<RealtimeEffectState> JuceAudioEngine::addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                             const std::string& effectId)
{
    return RealtimeEffectManager::Get(project).AddState(effectsInitialization(project), group, au3::wxFromStdString(effectId));
}

void JuceAudioEngine::removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                const std::shared_ptr<RealtimeEffectState>& state)
{
    RealtimeEffectManager::Get(project).RemoveState(effectsInitialization(project), group, state);
}

std::shared_ptr<RealtimeEffectState> JuceAudioEngine::replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                                 size_t effectListIndex,
                                                                                 const std::string& newEffectId)
{
    return RealtimeEffectManager::Get(project).ReplaceState(effectsInitialization(project), group, effectListIndex,
                                                            au3::wxFromStdString(newEffectId));
}

RealtimeEffects::InitializationScope* JuceAudioEngine::effectsInitialization(AudacityProject& project) const
{
    return m_stream && &m_stream->project == &project ? m_stream->effectsInitialization() : nullptr;
}

void JuceAudioEngine::setPlaybackMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter)
{
    m_playbackMeters[&project] = meter;
}

void JuceAudioEngine::setCaptureMeter(AudacityProject& project, const audio::IAudioMeterPtr& meter)
{
    m_captureMeters[&project] = meter;
}

void JuceAudioEngine::stopMeters(AudacityProject& project)
{
    for (const auto* meters : { &m_playbackMeters, &m_captureMeters }) {
        if (const auto meter = muse::value(*meters, &project)) {
            meter->stop();
        }
    }
}

void JuceAudioEngine::startMonitoring(AudacityProject& project)
{
    if (m_stream) {
        return;
    }
    const bool playThrough = recordConfiguration()->isInputMonitoringOn();
    if (m_monitoring) {
        if (&m_monitoring->project == &project && m_monitoring->playThrough == playThrough) {
            return;
        }
        stopMonitoring();
    }
    IF_ASSERT_FAILED(m_driverController) {
        return;
    }
    // As AudioIO, at the rate of the project. With the output even if silent: with input only, JUCE's ALSA devices stall
    // after their first callback.
    auto device = openDevice(m_driverController->configuration().inputChannels, numChannels, ProjectRate::Get(project).GetRate());
    if (!device) {
        LOGE() << "cannot monitor the input: " << m_lastError;
        return;
    }
    m_monitoring = std::make_unique<JuceMonitoringCallback>(project, std::move(device), playThrough,
                                                            muse::value(m_captureMeters, &project), m_inputVolume);
    m_monitoring->start();
}

void JuceAudioEngine::stopMonitoring()
{
    if (m_monitoring) {
        m_monitoring->stop();
        m_monitoring.reset();
    }
}

std::unique_ptr<juce::AudioIODevice> JuceAudioEngine::openDevice(int numInputChannels, int numOutputChannels, double sampleRate)
{
    IF_ASSERT_FAILED(m_driverController) {
        return nullptr;
    }
    const auto configuration = m_driverController->configuration();
    const auto type = m_devices->deviceType(configuration.api);
    if (!type) {
        m_lastError = muse::String::fromStdString("No such audio API: " + configuration.api);
        return nullptr;
    }
    const auto outputDevice = numOutputChannels > 0 ? effectiveOutputDevice(*m_devices, configuration) : std::string {};
    const auto inputDevice = numInputChannels > 0 ? effectiveInputDevice(*m_devices, configuration) : std::string {};
    std::unique_ptr<juce::AudioIODevice> device { type->createDevice(juce::String(outputDevice), juce::String(inputDevice)) };
    if (!device) {
        m_lastError = muse::String::fromStdString("Cannot create audio device " + outputDevice + " / " + inputDevice);
        return nullptr;
    }

    juce::BigInteger inputChannels;
    inputChannels.setRange(0, std::min(numInputChannels, device->getInputChannelNames().size()), true);
    juce::BigInteger outputChannels;
    outputChannels.setRange(0, std::min(numOutputChannels, device->getOutputChannelNames().size()), true);
    const auto rate = bestSampleRate(*device, sampleRate);
    const auto error = device->open(inputChannels, outputChannels, rate, bufferSize(*device, configuration.bufferLength, rate));
    if (error.isNotEmpty()) {
        m_lastError = muse::String::fromStdString(error.toStdString());
        return nullptr;
    }
    return device;
}

void JuceAudioEngine::setInputVolume(float newInputVolume)
{
    // Applied to the input, as AudioIO does where it can't set the volume of the device
    m_inputVolume = newInputVolume;
}

float JuceAudioEngine::getInputVolume() const
{
    return m_inputVolume;
}

void JuceAudioEngine::setPlaybackVolume(float newPlaybackVolume)
{
    m_playbackVolume = newPlaybackVolume;
}

float JuceAudioEngine::getPlaybackVolume() const
{
    return m_playbackVolume;
}

bool JuceAudioEngine::canStopTransport(AudacityProject& project) const
{
    return !m_stream || &m_stream->project == &project;
}

void JuceAudioEngine::handleDeviceChange()
{
    // The device is opened for each stream, and the driver controller suspends streams before changing it
}

muse::String JuceAudioEngine::lastErrorString() const
{
    return m_lastError;
}

double JuceAudioEngine::getPlaybackSampleRate() const
{
    return m_lastSampleRate;
}

void JuceAudioEngine::updateTimePosition(unsigned long newlyConsumedSamples)
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

std::optional<au::audio::AudioCallbackInfo> JuceAudioEngine::consumeNextCallbackInfo()
{
    audio::AudioCallbackInfo info;
    if (m_callbackInfoQueue.Get(info)) {
        return info;
    }
    return std::nullopt;
}

muse::async::Notification JuceAudioEngine::recordingUpdateRequested() const
{
    return m_listener->recordingUpdateRequested();
}

muse::async::Notification JuceAudioEngine::recordingNewBlocks() const
{
    return m_listener->recordingNewBlocks();
}

muse::async::Notification JuceAudioEngine::recordingCommitRequested() const
{
    return m_listener->recordingCommitRequested();
}

muse::async::Notification JuceAudioEngine::recordingFinished() const
{
    return m_listener->recordingFinished();
}

muse::async::Notification JuceAudioEngine::streamStopped() const
{
    return m_streamStopped;
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> JuceAudioEngine::recordingClipChanged() const
{
    return m_listener->recordingClipChanged();
}
