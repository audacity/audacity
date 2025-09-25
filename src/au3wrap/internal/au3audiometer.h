/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

#include "libraries/lib-audio-devices/IMeterSender.h"
#include <optional>

namespace au::au3 {
class Meter : public IMeterSender, public muse::async::Asyncable
{
public:
    Meter();

    void push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, int64_t key) override;
    void sendAll() override;
    void reset() override;
    void reserve(size_t size) override;
    void setSampleRate(double rate) override;

    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> dataChanged(int64_t key = MASTER_TRACK_ID);

private:
    struct Sample
    {
        float peak;
        float rms;
    };

    struct Data
    {
        int64_t key;
        au::audio::audioch_t channel;
        au::audio::AudioSignalVal peak;
        au::audio::AudioSignalVal rms;
    };

    Sample getSamplesMaxValue(const float* buffer, size_t frames, size_t step);
    void push(uint8_t channel, const Sample& sample, int64_t key);

    double m_sampleRate{ 44100.0 };
    const float m_smoothingCoef;
    std::vector<Data> m_trackData;
    std::map<int64_t, muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> > m_channels;
    std::map<int64_t, Sample> m_maxValue{};
    std::map<int64_t, std::optional<double> > m_lastSampleTimestamp{};
};
}
