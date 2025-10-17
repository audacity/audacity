/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iaudioengine.h"

namespace au::audio {
class AudioEngine : public IAudioEngine
{
public:
    AudioEngine() = default;

    void init();

    bool isBusy() const override;

    int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                    const AudioIOStartStreamOptions& options) override;

    void stopStream() override;
    void pauseStream(bool pause) override;

    void startMonitoring(const AudioIOStartStreamOptions& options) override;
    void stopMonitoring() override;

    muse::async::Notification updateRequested() const override;
    muse::async::Notification commitRequested() const override;
    muse::async::Notification finished() const override;
    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const override;
};
}
