/*
 * Audacity: A Digital Audio Editor
 */
#include "metersender.h"

#include "metertapnode.h"

using namespace au::au4audio;

MeterSender::MeterSender(std::vector<Tap> taps)
    : m_taps{std::move(taps)}
{
}

void MeterSender::send(audio::IAudioMeter& meter, int64_t index, size_t frames, audio::TimePoint time) const
{
    for (const auto& tap : m_taps) {
        const auto capture = tap.node->capture();
        tap.node->reset();
        // What is heard from `index`
        const auto from = index + tap.lead;
        if (capture.index > from || capture.index + static_cast<int64_t>(capture.frames) < from + static_cast<int64_t>(frames)) {
            continue;
        }
        for (size_t ch = 0; ch < capture.numChannels; ++ch) {
            meter.push(static_cast<uint8_t>(ch), { capture.channels[ch] + (from - capture.index), frames, 1, time }, tap.trackId);
        }
    }
}
