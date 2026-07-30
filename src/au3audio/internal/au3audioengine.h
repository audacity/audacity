/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"

#include "audio/iaudioengine.h"
#include "record/irecordconfiguration.h"

#include "au3-utility/Observer.h"

namespace au::au3audio {
class Au3AudioEngine final : public au::audio::IAudioEngine
{
public:
    Au3AudioEngine() = default;

    muse::GlobalInject<au::record::IRecordConfiguration> recordConfiguration;

    void init();
    void deinit();

    bool isBusy() const override;
    bool isCapturing() const override;
    bool isStreamActive(AudacityProject& project) const override;
    double streamTime() const override;
    bool isMonitoring() const override;
    std::optional<audio::AudioStreamDescriptor> currentStream() const override;

    int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, AudacityProject& project,
                    const StartStreamOptions& options) override;

    void stopStream() override;
    void pauseStream(bool pause) override;
    void seekStream(double time) override;

    std::shared_ptr<RealtimeEffectState> addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                const std::string& effectId) override;
    void removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                   const std::shared_ptr<RealtimeEffectState>& state) override;
    std::shared_ptr<RealtimeEffectState> replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group, size_t effectListIndex,
                                                                    const std::string& newEffectId) override;

    void startMonitoring(AudacityProject& project) override;
    void stopMonitoring() override;

    void setInputVolume(float newInputVolume) override;
    float getInputVolume() const override;
    void setPlaybackVolume(float newPlaybackVolume) override;
    float getPlaybackVolume() const override;

    bool canStopAudioStream(AudacityProject& project) const override;

    void handleDeviceChange() override;
    muse::String lastErrorString() const override;
    double getPlaybackSampleRate() const override;
    void updateTimePosition(unsigned long newlyConsumedSamples) override;
    std::optional<au::audio::AudioCallbackInfo> consumeNextCallbackInfo() override;

    muse::async::Notification recordingUpdateRequested() const override;
    muse::async::Notification recordingCommitRequested() const override;
    muse::async::Notification recordingFinished() const override;
    muse::async::Notification streamStopped() const override;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const override;

private:
    Observer::Subscription m_streamStatusSubscription;
    muse::async::Notification m_streamStopped;
};
}
