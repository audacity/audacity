/*
 * Audacity: A Digital Audio Editor
 */
#include "au3audiometer.h"

namespace au::au3 {
Au3AudioMeter::Au3AudioMeter(std::unique_ptr<audio::IAudioMeter> audioMeter)
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

muse::async::Channel<audio::audioch_t, audio::MeterSignal> Au3AudioMeter::dataChanged(
    const std::optional<audio::IAudioMeter::TrackId>& trackId)
{
    return m_audioMeter->dataChanged(trackId);
}
}
