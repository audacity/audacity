/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "audiometer.h"
#include "auqttimer.h"

#include "global/log.h"
#include "global/types/ratio.h" // muse::linear_to_db

namespace au::audio {
namespace {
constexpr double updatePeriod = 1 / 30.0;
constexpr auto decayDbPerSecond = 36.0f;
constexpr int64_t MASTER_TRACK_ID = -2;
}

std::unique_ptr<IAudioMeter> IAudioMeter::create()
{
    auto playingTimer = std::make_unique<AuQtTimer>(Qt::PreciseTimer);
    auto stoppingTimer = std::make_unique<AuQtTimer>(Qt::VeryCoarseTimer);
    return std::make_unique<AudioMeter>(std::move(playingTimer), std::move(stoppingTimer));
}

void AudioMeter::decay(LevelState& level)
{
    constexpr float decayDb = decayDbPerSecond * updatePeriod;
    if (level.hangover == 0) {
        level.db = std::max(level.db - decayDb, leastDb);
    } else {
        --level.hangover;
    }
}

void AudioMeter::maybeBumpUp(LevelState& level, float newLinValue, int hangover)
{
    const auto newPeakDb = muse::linear_to_db(newLinValue);
    if (level.db < newPeakDb) {
        level.db = newPeakDb;
        level.hangover = hangover;
    }
}

AudioMeter::AudioMeter(std::unique_ptr<ITimer> playingTimer, std::unique_ptr<ITimer> stoppingTimer)
    : m_playingTimer{std::move(playingTimer)}, m_stoppingTimer{std::move(stoppingTimer)}
{
    constexpr int letRingMs = -1000 * leastDb / decayDbPerSecond;
    static_assert(letRingMs > 0);
    m_stoppingTimer->setSingleShot(true);
    m_stoppingTimer->setInterval(std::chrono::milliseconds { letRingMs });
    m_stoppingTimer->setCallback([this]() {
        m_playingTimer->stop();
        for (auto& [_, trackData] : m_trackData) {
            trackData.channelLevels.clear();
        }
    });

    m_playingTimer->setInterval(std::chrono::milliseconds { static_cast<int>(updatePeriod * 1000) });
    m_playingTimer->setCallback([this]() {
        for (auto& [_, trackData] : m_trackData) {
            for (auto& [_, levels] : trackData.channelLevels) {
                decay(levels.peak);
                decay(levels.rms);
            }
        }

        QueueItem item;
        while (m_lockFreeQueue.Get(item)) {
            m_mainThreadQueue.emplace(item);
        }

        const auto now = std::chrono::steady_clock::now();
        while (!m_mainThreadQueue.empty() && now >= m_mainThreadQueue.front().dacTime) {
            const QueueItem& item = m_mainThreadQueue.front();
            auto& levels = m_trackData[item.trackId].channelLevels[item.channel];
            maybeBumpUp(levels.peak, item.sample.peak, m_hangoverCount.load());
            maybeBumpUp(levels.rms, item.sample.rms, m_hangoverCount.load());
            m_mainThreadQueue.pop();
        }

        for (std::pair<const TrackId, TrackData>& entry : m_trackData) {
            auto& trackData = entry.second;
            for (const std::pair<const audioch_t, Levels>& entry : trackData.channelLevels) {
                const auto& [channel, levels] = entry;
                trackData.notificationChannel.send(channel, MeterSignal {
                        { muse::db_to_linear(levels.peak.db), levels.peak.db },
                        { muse::db_to_linear(levels.rms.db), levels.rms.db } });
            }
        }
    });
}

AudioMeter::QueueSample AudioMeter::getSamplesMaxValue(const float* buffer, size_t frames, size_t step)
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

    return AudioMeter::QueueSample{ std::min(peak, 1.0f), std::min(rms, 1.0f) };
}

void AudioMeter::push(uint8_t channel, const InterleavedSampleData& sampleData, const std::optional<TrackId>& trackId)
{
    if (!m_running.load()) {
        if (!m_warningIssued) {
            m_warningIssued = true;
            LOGW() << "AudioMeter::push called while not running";
        }
        return;
    }
    if (static_cast<int>(sampleData.frames) > m_maxFramesPerPush) {
        m_maxFramesPerPush = static_cast<int>(sampleData.frames);
        const auto hangoverTime = m_maxFramesPerPush / m_sampleRate;
        m_hangoverCount.store(std::ceil(hangoverTime / updatePeriod));
    }
    const QueueSample value = getSamplesMaxValue(sampleData.buffer, sampleData.frames, sampleData.nChannels);
    m_lockFreeQueue.Put(QueueItem { trackId.value_or(TrackId { MASTER_TRACK_ID }), channel, value, sampleData.dacTime });
}

void AudioMeter::start(double sampleRate)
{
    m_stoppingTimer->stop();
    for (auto& [trackId, trackData] : m_trackData) {
        trackData.channelLevels.clear();
    }

    m_playingTimer->start();
    m_sampleRate = sampleRate;
    m_running.store(true);
    m_maxFramesPerPush = 0;
}

void AudioMeter::stop()
{
    m_warningIssued = false;
    m_running.store(false);
    m_lockFreeQueue.Clear();
    decltype(m_mainThreadQueue) emptyQueue;
    m_mainThreadQueue.swap(emptyQueue);
    m_stoppingTimer->start();
}

muse::async::Channel<audioch_t, MeterSignal> AudioMeter::dataChanged(const std::optional<TrackId>& oTrackId)
{
    const auto trackId = oTrackId.value_or(TrackId { MASTER_TRACK_ID });
    auto& channel = m_trackData[trackId].notificationChannel;
    channel.onClose(this, [this, trackId]() {
        m_trackData.erase(trackId);
    }, muse::async::Asyncable::Mode::SetReplace);

    return channel;
}
}
