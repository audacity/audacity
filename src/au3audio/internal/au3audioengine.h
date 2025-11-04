/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "audio/iaudioengine.h"
#include "record/irecordconfiguration.h"

namespace au::au3audio {
class Au3AudioEngine final : public au::audio::IAudioEngine
{
public:
    Au3AudioEngine() = default;

    muse::Inject<au::record::IRecordConfiguration> recordConfiguration;

    void init();
    void deinit();

    bool isBusy() const override;

    int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, AudacityProject& project,
                    bool isDefaultPlayTrackPolicy, double audioStreamSampleRate) override;

    void stopStream() override;
    void pauseStream(bool pause) override;
    void seekStream(double time) override;

    void startMonitoring(AudacityProject& project) override;
    void stopMonitoring() override;

    void setInputVolume(float newInputVolume) override;
    void getInputVolume(float& inputVolume) const override;
    void setPlaybackVolume(float newPlaybackVolume) override;
    void getPlaybackVolume(float& playbackVolume) const override;

    bool canStopAudioStream(AudacityProject& project) const override;

    void handleDeviceChange() override;
    int getHostIndex(const std::string& hostName) override;
    muse::String lastErrorString() const override;
    double getPlaybackSampleRate() const override;
    void updateTimePosition(unsigned long newlyConsumedSamples) override;
    std::optional<AudioCallbackInfo> consumeNextCallbackInfo() override;

    muse::async::Notification updateRequested() const override;
    muse::async::Notification commitRequested() const override;
    muse::async::Notification finished() const override;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const override;
};
}
