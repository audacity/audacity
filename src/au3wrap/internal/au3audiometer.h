/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "auaudio/iaudiometer.h"
#include "libraries/lib-audio-devices/IMeterSender.h"

namespace au::au3 {
/**
 * @brief Adapter class to wrap an IAudioMeter as an IMeterSender
 */
class Au3AudioMeter : public IMeterSender
{
public:
    Au3AudioMeter(std::unique_ptr<auaudio::IAudioMeter> audioMeter);

    void push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, TrackId) override;
    void start(double sampleRate) override;
    void stop() override;

    muse::async::Channel<auaudio::audioch_t, auaudio::MeterSignal> dataChanged(TrackId key = TrackId { MASTER_TRACK_ID });

private:
    const std::unique_ptr<auaudio::IAudioMeter> m_audioMeter;
};
}
