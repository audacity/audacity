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

    muse::async::Notification updateRequested() const override;
    muse::async::Notification commitRequested() const override;
    muse::async::Notification finished() const override;
};
}
