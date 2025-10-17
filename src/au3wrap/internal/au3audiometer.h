/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

// TODO
// For now we include from auaudio internals because this class implements an Au3 interface (`IMeterSender`).
// We should wrap that interface in an Au3-independent version that we can then have declared in auaudio.
#include "auaudio/internal/itimer.h"

#include "libraries/lib-audio-devices/IMeterSender.h"
#include "libraries/lib-utility/LockFreeQueue.h"

#include <QTimer>

#include <atomic>
#include <map>
#include <unordered_map>

namespace au::au3 {
class Meter : public IMeterSender, public muse::async::Asyncable
{
public:
    Meter(std::unique_ptr<ITimer> meterUpdateTimer, std::unique_ptr<ITimer> stopTimer);

    void push(uint8_t channel, const IMeterSender::InterleavedSampleData& sampleData, TrackId) override;
    void start() override;
    void stop() override;
    void setSampleRate(double rate) override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> dataChanged(TrackId key = TrackId { MASTER_TRACK_ID });

private:
    struct QueueSample
    {
        float peak = 0.f;
        float rms = 0.f;
    };

    struct QueueItem
    {
        TrackId trackId;
        audio::audioch_t channel;
        QueueSample sample;
    };

    static constexpr auto leastDb = -100.0f;

    struct LevelState {
        float db = leastDb;
        int hangover = 0;
    };

    struct Levels
    {
        LevelState peak;
        LevelState rms;
    };

    struct TrackData {
        muse::async::Channel<audio::audioch_t, audio::MeterSignal> notificationChannel;
        std::unordered_map<audio::audioch_t, Levels> channelLevels;
    };

    static QueueSample getSamplesMaxValue(const float* buffer, size_t frames, size_t step);
    static void decay(LevelState&);
    static void maybeBumpUp(LevelState&, float newLinValue, int hangover);

    double m_sampleRate{ 44100.0 };
    std::atomic<int> m_hangoverCount = 0;
    int m_maxFramesPerPush = 0;
    LockFreeQueue<QueueItem> m_queue{ 1024 };
    std::map<TrackId, TrackData> m_trackData;
    const std::unique_ptr<ITimer> m_meterUpdateTimer;
    const std::unique_ptr<ITimer> m_stopTimer;
    std::atomic<bool> m_running { false };
    bool m_stopPending = true;
    bool m_warningIssued = false;
};
}
