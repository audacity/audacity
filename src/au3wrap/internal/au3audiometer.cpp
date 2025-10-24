/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiometer.h"

#include "global/async/async.h"
#include "global/log.h"

#include "libraries/lib-utility/MathApprox.h"
#include "libraries/lib-utility/MemoryX.h"

#include "global/types/ratio.h" // muse::linear_to_db

#include <cmath> // std::ceil

namespace au::au3 {
namespace {
constexpr double updatePeriod = 1 / 30.0;
constexpr auto decayDbPerSecond = 36.0f;
}

void Meter::decay(LevelState& level)
{
    constexpr float decayDb = decayDbPerSecond * updatePeriod;
    if (level.hangover == 0) {
        level.db = std::max(level.db - decayDb, leastDb);
    } else {
        --level.hangover;
    }
}

void Meter::maybeBumpUp(LevelState& level, float newLinValue, int hangover)
{
    const auto newPeakDb = muse::linear_to_db(newLinValue);
    if (level.db < newPeakDb) {
        level.db = newPeakDb;
        level.hangover = hangover;
    }
}

Meter::Meter()
{
    constexpr int letRingMs = -1000 * leastDb / decayDbPerSecond;
    static_assert(letRingMs > 0);
    m_stopTimer.setSingleShot(true);
    m_stopTimer.setInterval(letRingMs);
    m_stopTimer.callOnTimeout([this]() {
        m_meterUpdateTimer.stop();
        for (auto& [_, trackData] : m_trackData) {
            trackData.channelLevels.clear();
        }
    });

    m_meterUpdateTimer.setInterval(static_cast<int>(updatePeriod * 1000));

    m_meterUpdateTimer.callOnTimeout([this]() {
        for (auto& [_, trackData] : m_trackData) {
            for (auto& [_, levels] : trackData.channelLevels) {
                decay(levels.peak);
                decay(levels.rms);
            }
        }

        QueueItem item;
        while (m_queue.Get(item)) {
            auto& levels = m_trackData[item.trackId].channelLevels[item.channel];
            maybeBumpUp(levels.peak, item.sample.peak, m_hangoverCount.load());
            maybeBumpUp(levels.rms, item.sample.rms, m_hangoverCount.load());
        }

        for (auto& [_, trackData] : m_trackData) {
            for (const auto& [channel, levels] : trackData.channelLevels) {
                trackData.notificationChannel.send(channel, audio::MeterSignal { levels.peak.db, levels.rms.db });
            }
        }
    });
}

Meter::QueueSample Meter::getSamplesMaxValue(const float* buffer, size_t frames, size_t step)
{
    const auto* sptr = buffer;
    float peak = 0.0f;
    float rms = 0.0f;

    for (unsigned long i = 0; i < frames; i++) {
        peak = std::max(peak, std::fabs(*sptr));
        rms += (*sptr) * (*sptr);
        sptr += step;
    }

    rms = std::sqrt(rms / static_cast<float>(frames));

    return Meter::QueueSample{ std::min(peak, 1.0f), std::min(rms, 1.0f) };
}

void Meter::push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, TrackId trackId)
{
    if (!m_running.load()) {
        if (!m_warningIssued) {
            m_warningIssued = true;
            LOGW() << "Meter::push called while not running";
        }
        return;
    }
    if (static_cast<int>(sampleData.frames) > m_maxFramesPerPush) {
        m_maxFramesPerPush = sampleData.frames;
        const auto hangoverTime = m_maxFramesPerPush / m_sampleRate;
        m_hangoverCount.store(std::ceil(hangoverTime / updatePeriod));
    }
    const QueueSample value = getSamplesMaxValue(sampleData.buffer, sampleData.frames, sampleData.nChannels);
    m_queue.Put(QueueItem { trackId, channel, value });
}

void Meter::start()
{
    if (!m_meterUpdateTimer.isActive()) {
        m_meterUpdateTimer.start();
    }
    m_stopTimer.stop();
    m_running.store(true);
    m_maxFramesPerPush = 0;
}

void Meter::stop()
{
    m_warningIssued = false;
    m_running.store(false);
    m_queue.Clear();
    m_stopTimer.start();
}

void Meter::setSampleRate(double rate)
{
    m_sampleRate = rate;
}

muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> Meter::dataChanged(TrackId trackId)
{
    auto& channel = m_trackData[trackId].notificationChannel;
    channel.onClose(this, [this, trackId]() {
        m_trackData.erase(trackId);
    }, muse::async::Asyncable::Mode::SetReplace);

    return channel;
}
}
