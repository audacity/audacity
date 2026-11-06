/*
 * Audacity: A Digital Audio Editor
 */
#include "au3audiometer.h"

namespace au::au3audio {
Au3AudioMeter::Au3AudioMeter(audio::IAudioMeterPtr audioMeter)
    : m_audioMeter{std::move(audioMeter)}
{
}

void Au3AudioMeter::push(uint8_t channel, const InterleavedSampleData& au3SampleData, const std::optional<TrackId>& trackId)
{
    audio::IAudioMeter::InterleavedSampleData sampleData {
        au3SampleData.buffer,
        au3SampleData.frames,
        au3SampleData.nChannels,
        au3SampleData.dacTime
    };
    m_audioMeter->push(channel, std::move(sampleData), trackId ? std::make_optional<audio::IAudioMeter::TrackId>(
                           trackId->value) : std::nullopt);
}

void Au3AudioMeter::start(double sampleRate)
{
    m_audioMeter->start(sampleRate);
}

void Au3AudioMeter::stop()
{
    m_audioMeter->stop();
}
}
