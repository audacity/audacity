#include "au3audioengine.h"

#include "framework/global/realfn.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"

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
}

void AudioEngine::deinit()
{
    AudioIO::Deinit();
}

bool Au3AudioEngine::isBusy() const
{
    return AudioIO::Get()->IsBusy();
}

int Au3AudioEngine::startStream(const TransportSequences& sequences, const double startTime, const double endTime,
                                const double mixerEndTime,
                                AudacityProject& project, const bool isDefaultPlayTrackPolicy, const double audioStreamSampleRate)
{
    AudioIOStartStreamOptions options = ProjectAudioIO::GetDefaultOptions(project, isDefaultPlayTrackPolicy);
    options.inputMonitoring = recordConfiguration()->isInputMonitoringOn();
    options.rate = audioStreamSampleRate;
    return AudioIO::Get()->StartStream(sequences, startTime, endTime, mixerEndTime, options);
}

void Au3AudioEngine::stopStream()
{
    AudioIO::Get()->StopStream();
    AudioIO::Get()->WaitWhileBusy();
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

void Au3AudioEngine::getInputVolume(float& inputVolume) const
{
    int inputSource;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
}

void Au3AudioEngine::setPlaybackVolume(const float newPlaybackVolume)
{
    int inputSource;
    float inputVolume;
    float playbackVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
    AudioIO::Get()->SetMixer(inputSource, inputVolume, newPlaybackVolume);
}

void Au3AudioEngine::getPlaybackVolume(float& playbackVolume) const
{
    int inputSource;
    float inputVolume;
    AudioIO::Get()->GetMixer(&inputSource, &inputVolume, &playbackVolume);
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

int Au3AudioEngine::getHostIndex(const std::string& hostName)
{
    return AudioIO::Get()->GetHostIndex(hostName);
}

muse::String Au3AudioEngine::lastErrorString() const
{
    return au::au3::wxToString(AudioIO::Get()->LastPaErrorString());
}

muse::async::Notification Au3AudioEngine::updateRequested() const
{
    return s_audioIOListener->updateRequested();
}

muse::async::Notification Au3AudioEngine::commitRequested() const
{
    return s_audioIOListener->commitRequested();
}

muse::async::Notification Au3AudioEngine::finished() const
{
    return s_audioIOListener->finished();
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> Au3AudioEngine::recordingClipChanged() const
{
    return s_audioIOListener->recordingClipChanged();
}
