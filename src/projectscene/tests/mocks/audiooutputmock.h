/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "playback/iaudiooutput.h"

namespace au::playback {
class AudioOutputMock : public IAudioOutput
{
public:
    MOCK_METHOD(float, playbackVolume, (), (const, override));
    MOCK_METHOD(void, setPlaybackVolume, (float), (override));
    MOCK_METHOD(muse::async::Channel<float>, playbackVolumeChanged, (), (const, override));

    MOCK_METHOD(audio::sample_rate_t, sampleRate, (), (const, override));
    MOCK_METHOD(muse::async::Channel<audio::sample_rate_t>, sampleRateChanged, (), (const, override));

    MOCK_METHOD((muse::async::Channel<audio::audioch_t, audio::MeterSignal>), playbackSignalChanges, (), (const, override));
    MOCK_METHOD((muse::async::Channel<audio::audioch_t, audio::MeterSignal>), playbackTrackSignalChanges, (int64_t),
                (const, override));
};
}
