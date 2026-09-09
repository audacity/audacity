#include "au3audioengine.h"

#include "framework/global/log.h"
#include "framework/global/realfn.h"

#include "au3-audio-io/AudioIO.h"
#include "au3-audio-io/ProjectAudioIO.h"
#include "au3-track/Track.h"
#include "au3-time-frequency-selection/ViewInfo.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "defaultplaybackpolicy.h"
#include "au3audioiolistener.h"

using namespace au::au3audio;

std::shared_ptr<Au3AudioIOListener> s_audioIOListener;

static ProjectAudioIO::DefaultOptions::Scope s_defaultOptionsScope {
    [](au::au3::Au3Project& project, const bool newDefault) -> AudioIOStartStreamOptions {
        auto options = ProjectAudioIO::DefaultOptionsFactory(project, newDefault);
        options.listener = s_audioIOListener;

        const auto& playRegion = ViewInfo::Get(project).playRegion;
        const bool loopEnabled = playRegion.Active();
        options.loopEnabled = loopEnabled;

        if (newDefault) {
            const double trackEndTime = TrackList::Get(project).GetEndTime();
            const double loopEndTime = playRegion.GetEnd();
            options.policyFactory = [&project, trackEndTime, loopEndTime](
                const AudioIOStartStreamOptions& options) -> std::unique_ptr<PlaybackPolicy>
            {
                return std::make_unique<DefaultPlaybackPolicy>(project,
                                                               trackEndTime, loopEndTime, options.pStartTime,
                                                               options.loopEnabled, options.variableSpeed);
            };

            double startTime = playRegion.GetStart();
            options.pStartTime.emplace(muse::RealIsEqualOrMore(startTime, 0.0) ? startTime : 0.0);
        }

        return options;
    } };

void Au3AudioEngine::init()
{
    s_audioIOListener = std::make_shared<Au3AudioIOListener>();
    AudioIO::Init();

    m_streamStatusSubscription = AudioIO::Get()->Subscribe([this](const AudioIOEvent& event) {
        if (!event.on && event.type != AudioIOEvent::PAUSE) {
            m_streamStopped.notify();
        }
    });
}

void Au3AudioEngine::deinit()
{
    m_streamStatusSubscription.Reset();
    AudioIO::Deinit();
}

bool Au3AudioEngine::isBusy() const
{
    return AudioIO::Get()->IsBusy();
}

bool Au3AudioEngine::isCapturing() const
{
    return AudioIO::Get()->IsCapturing();
}

bool Au3AudioEngine::isStreamActive(AudacityProject& project) const
{
    const int token = ProjectAudioIO::Get(project).GetAudioIOToken();
    return AudioIO::Get()->IsStreamActive(token);
}

double Au3AudioEngine::streamTime() const
{
    return AudioIO::Get()->GetStreamTime();
}

bool Au3AudioEngine::isMonitoring() const
{
    return AudioIO::Get()->IsMonitoring();
}

std::optional<au::audio::AudioStreamDescriptor> Au3AudioEngine::currentStream() const
{
    const auto audioIO = AudioIO::Get();
    if (!audioIO->IsBusy() && !audioIO->IsStreamActive() && !audioIO->IsMonitoring()) {
        return std::nullopt;
    }

    const auto owner = audioIO->GetOwningProject();

    au::audio::AudioStreamKind kind = au::audio::AudioStreamKind::Playback;
    if (audioIO->IsMonitoring()) {
        kind = au::audio::AudioStreamKind::Monitoring;
    } else if (audioIO->GetNumCaptureChannels() > 0) {
        // Recording lead-in has capture channels before IsCapturing() becomes true.
        kind = au::audio::AudioStreamKind::Recording;
    }

    return au::audio::AudioStreamDescriptor {
        kind,
        owner.get(),
        audioIO->GetPlaybackSampleRate(),
    };
}

int Au3AudioEngine::startStream(const TransportSequences& sequences, const double startTime, const double endTime,
                                const double mixerEndTime,
                                AudacityProject& project, const StartStreamOptions& options)
{
    AudioIOStartStreamOptions au3Options = ProjectAudioIO::GetDefaultOptions(project, options.isDefaultPolicy);
    au3Options.inputMonitoring = recordConfiguration()->isInputMonitoringOn();
    au3Options.rate = options.sampleRate;
    au3Options.leadInTime = options.leadInTime;
    if (options.crossfadeData) {
        au3Options.pCrossfadeData = options.crossfadeData;
    }
    if (options.streamStartTime.has_value()) {
        au3Options.pStartTime = *options.streamStartTime;
    }
    auto& audioIO = *AudioIO::Get();
    const int token = audioIO.StartStream(sequences, startTime, endTime, mixerEndTime, au3Options);
    if (token > 0) {
        ProjectAudioIO::Get(project).SetAudioIOToken(token);
        LOGI() << "PortAudio latency report (ms): outputLatency=" << audioIO.GetHardwarePlaybackLatencyMs() <<
            ", inputLatency=" << audioIO.GetHardwareCaptureLatencyMs();
    }
    return token;
}

void Au3AudioEngine::stopStream()
{
    AudioIO::Get()->StopStream();
    AudioIO::Get()->WaitWhileBusy();
}

std::shared_ptr<RealtimeEffectState> Au3AudioEngine::addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                            const std::string& effectId)
{
    return AudioIO::Get()->AddState(project, group, effectId);
}

void Au3AudioEngine::removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                               const std::shared_ptr<RealtimeEffectState>& state)
{
    AudioIO::Get()->RemoveState(project, group, state);
}

std::shared_ptr<RealtimeEffectState> Au3AudioEngine::replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                                size_t effectListIndex, const std::string& newEffectId)
{
    return AudioIO::Get()->ReplaceState(project, group, effectListIndex, newEffectId);
}

void Au3AudioEngine::pauseStream(const bool pause)
{
    AudioIO::Get()->SetPaused(pause);
}

void Au3AudioEngine::seekStream(double time)
{
    AudioIO::Get()->SeekStream(time - AudioIO::Get()->GetStreamTime());
}

void Au3AudioEngine::startMonitoring(AudacityProject& project)
{
    AudioIOStartStreamOptions options = ProjectAudioIO::GetDefaultOptions(project);
    options.inputMonitoring = recordConfiguration()->isInputMonitoringOn();
    AudioIO::Get()->StartMonitoring(options);
}

void Au3AudioEngine::stopMonitoring()
{
    AudioIO::Get()->StopMonitoring();
}

void Au3AudioEngine::setInputVolume(const float newInputVolume)
{
    int inputSource;
    float inputVolume;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
    AudioIO::Get()->SetMixer(inputSource, newInputVolume, playbackVolume);
}

float Au3AudioEngine::getInputVolume() const
{
    int inputSource;
    float inputVolume;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
    return inputVolume;
}

void Au3AudioEngine::setPlaybackVolume(const float newPlaybackVolume)
{
    int inputSource;
    float inputVolume;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
    AudioIO::Get()->SetMixer(inputSource, inputVolume, newPlaybackVolume);
}

float Au3AudioEngine::getPlaybackVolume() const
{
    int inputSource;
    float inputVolume;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
    return playbackVolume;
}

bool Au3AudioEngine::canStopAudioStream(AudacityProject& project) const
{
    return !AudioIO::Get()->IsStreamActive()
           || AudioIO::Get()->IsMonitoring()
           || AudioIO::Get()->GetOwningProject().get() == &project;
}

void Au3AudioEngine::handleDeviceChange()
{
    AudioIO::Get()->HandleDeviceChange();
}

muse::String Au3AudioEngine::lastErrorString() const
{
    return au::au3::wxToString(AudioIO::Get()->LastPaErrorString());
}

double Au3AudioEngine::getPlaybackSampleRate() const
{
    return AudioIO::Get()->GetPlaybackSampleRate();
}

void Au3AudioEngine::updateTimePosition(const unsigned long newlyConsumedSamples)
{
    AudioIO::Get()->UpdateTimePosition(newlyConsumedSamples);
}

std::optional<au::audio::AudioCallbackInfo> Au3AudioEngine::consumeNextCallbackInfo()
{
    AudioIoCallback::AudioCallbackInfo au3Info;
    if (AudioIO::Get()->GetAudioCallbackInfoQueue().Get(au3Info)) {
        return au::audio::AudioCallbackInfo { au3Info.dacTime, au3Info.numSamples };
    }
    return std::nullopt;
}

muse::async::Notification Au3AudioEngine::recordingUpdateRequested() const
{
    return s_audioIOListener->recordingUpdateRequested();
}

muse::async::Notification Au3AudioEngine::recordingCommitRequested() const
{
    return s_audioIOListener->recordingCommitRequested();
}

muse::async::Notification Au3AudioEngine::recordingFinished() const
{
    return s_audioIOListener->recordingFinished();
}

muse::async::Notification Au3AudioEngine::streamStopped() const
{
    return m_streamStopped;
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> Au3AudioEngine::recordingClipChanged() const
{
    return s_audioIOListener->recordingClipChanged();
}
