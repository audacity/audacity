#include "audioengine.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"

#include "au3wrap/au3types.h"

#include "defaultplaybackpolicy.h"
#include "au3audioiolistener.h"

#include "realfn.h"

using namespace au::audio;

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

void AudioEngine::init()
{
    s_audioIOListener = std::make_shared<Au3AudioIOListener>();
}

bool AudioEngine::isBusy() const
{
    return AudioIO::Get()->IsBusy();
}

int AudioEngine::startStream(const TransportSequences& sequences, const double startTime, const double endTime, const double mixerEndTime,
                             AudacityProject& project, const bool isDefaultPlayTrackPolicy, const double audioStreamSampleRate)
{
    AudioIOStartStreamOptions options = ProjectAudioIO::GetDefaultOptions(project, isDefaultPlayTrackPolicy);
    options.inputMonitoring = recordConfiguration()->isInputMonitoringOn();
    options.rate = audioStreamSampleRate;
    return AudioIO::Get()->StartStream(sequences, startTime, endTime, mixerEndTime, options);
}

void AudioEngine::stopStream()
{
    AudioIO::Get()->StopStream();
    AudioIO::Get()->WaitWhileBusy();
}

void AudioEngine::pauseStream(const bool pause)
{
    AudioIO::Get()->SetPaused(pause);
}

void AudioEngine::startMonitoring(AudacityProject& project)
{
    AudioIOStartStreamOptions options = ProjectAudioIO::GetDefaultOptions(project);
    options.inputMonitoring = recordConfiguration()->isInputMonitoringOn();
    AudioIO::Get()->StartMonitoring(options);
}

void AudioEngine::stopMonitoring()
{
    AudioIO::Get()->StopMonitoring();
}

muse::async::Notification AudioEngine::updateRequested() const
{
    return s_audioIOListener->updateRequested();
}

muse::async::Notification AudioEngine::commitRequested() const
{
    return s_audioIOListener->commitRequested();
}

muse::async::Notification AudioEngine::finished() const
{
    return s_audioIOListener->finished();
}

muse::async::Channel<au::au3::Au3TrackId, au::au3::Au3ClipId> AudioEngine::recordingClipChanged() const
{
    return s_audioIOListener->recordingClipChanged();
}
