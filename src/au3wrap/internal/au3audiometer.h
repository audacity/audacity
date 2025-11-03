/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "audio/iaudiometer.h"
#include "libraries/lib-audio-devices/IMeterSender.h"

namespace au::au3 {
/**
 * @brief Adapter class to wrap an IAudioMeter as an IMeterSender
 */
class Au3AudioMeter : public IMeterSender
{
public:
    Au3AudioMeter(std::unique_ptr<audio::IAudioMeter> audioMeter);

    void push(uint8_t channel, const InterleavedSampleData& sampleData, const std::optional<TrackId>&) override;
    void start(double sampleRate) override;
    void stop() override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> dataChanged(
        const std::optional<audio::IAudioMeter::TrackId>& trackId = std::nullopt);

private:
    const std::unique_ptr<audio::IAudioMeter> m_audioMeter;
};
}
