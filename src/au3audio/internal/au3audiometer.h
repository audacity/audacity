/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "audio/iaudiometer.h"
#include "au3-audio-devices/IMeterSender.h"

namespace au::au3audio {
/**
 * @brief Adapter class to wrap an IAudioMeter as an IMeterSender
 */
class Au3AudioMeter : public IMeterSender
{
public:
    explicit Au3AudioMeter(audio::IAudioMeterPtr audioMeter);

    void push(uint8_t channel, const InterleavedSampleData& sampleData, const std::optional<TrackId>&) override;
    void start(double sampleRate) override;
    void stop() override;

private:
    const audio::IAudioMeterPtr m_audioMeter;
};
}
