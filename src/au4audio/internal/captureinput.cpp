/*
* Audacity: A Digital Audio Editor
*/
#include "captureinput.h"

#include <algorithm>
#include <cmath>

using namespace au::au4audio;

namespace {
//! The input meter shows at most stereo
constexpr size_t maxMainMeterChannels = 2;
}

CaptureInput::CaptureInput(int numChannels, int maxFrames)
    : m_numChannels{numChannels},
    m_interleaved(static_cast<size_t>(numChannels * maxFrames)),
    m_mainMeter(static_cast<size_t>(maxMainMeterChannels * maxFrames))
{
}

const float* CaptureInput::read(const float* const* input, int frames, float volume)
{
    m_frames = frames;
    for (int ch = 0; ch < m_numChannels; ++ch) {
        for (int i = 0; i < frames; ++i) {
            m_interleaved[i * m_numChannels + ch] = input[ch] ? input[ch][i] * volume : 0.f;
        }
    }
    return m_interleaved.data();
}

void CaptureInput::sendToMeter(audio::IAudioMeter& meter, const RecordableSequences& sequences, audio::TimePoint time)
{
    const auto frames = static_cast<size_t>(m_frames);
    const auto numChannels = static_cast<size_t>(m_numChannels);
    const float* const samples = m_interleaved.data();

    // The meter of each track recorded into, wrapping around the input's channels if the track has more
    for (const auto& sequence : sequences) {
        const auto id = audio::IAudioMeter::TrackId { sequence->GetRecordableSequenceId() };
        for (size_t ch = 0; ch < sequence->NChannels(); ++ch) {
            meter.push(static_cast<uint8_t>(ch), { samples + ch % numChannels, frames, numChannels, time }, id);
        }
    }

    // The main meter
    if (numChannels <= maxMainMeterChannels) {
        for (size_t ch = 0; ch < numChannels; ++ch) {
            meter.push(static_cast<uint8_t>(ch), { samples + ch, frames, numChannels, time });
        }
        return;
    }
    // The loudest of the input's channels, alternately left and right
    std::fill_n(m_mainMeter.begin(), maxMainMeterChannels * frames, 0.f);
    for (size_t i = 0; i < frames; ++i) {
        for (size_t ch = 0; ch < numChannels; ++ch) {
            auto& peak = m_mainMeter[(ch % maxMainMeterChannels) * frames + i];
            const auto sample = samples[i * numChannels + ch];
            if (std::abs(sample) > std::abs(peak)) {
                peak = sample;
            }
        }
    }
    for (size_t ch = 0; ch < maxMainMeterChannels; ++ch) {
        meter.push(static_cast<uint8_t>(ch), { m_mainMeter.data() + ch * frames, frames, 1, time });
    }
}

void CaptureInput::playThrough(float* const* output, int numOutputChannels, int offset) const
{
    constexpr int maxPlayThroughChannels = 2;
    for (int ch = 0; ch < std::min(numOutputChannels, maxPlayThroughChannels); ++ch) {
        const auto source = m_numChannels == 1 ? 0 : ch;
        if (!output[ch] || source >= m_numChannels) {
            continue;
        }
        for (int i = 0; i < m_frames; ++i) {
            output[ch][offset + i] += m_interleaved[i * m_numChannels + source];
        }
    }
}
