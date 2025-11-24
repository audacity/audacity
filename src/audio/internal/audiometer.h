/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/channel.h"

#include "audio/iaudiometer.h"
#include "audio/internal/itimer.h"

#include "au3-utility/LockFreeQueue.h"

#include <QTimer>

#include <atomic>
#include <map>
#include <unordered_map>
#include <queue>

namespace au::audio {
class AudioMeter : public IAudioMeter, public muse::async::Asyncable
{
public:
    /**
     * @param playingTimer For the update of the meters. Called very frequently and needs high precision to maximize smoothness of meter animation.
     * @param stoppingTimer After pressing stop, `playingTimer` will continue running for a while until the meters have decayed and become invisible.
     * The `stoppingTimer` will tell when `playingTimer` can definitely stop. May be very coarse.
     */
    AudioMeter(std::unique_ptr<ITimer> playingTimer, std::unique_ptr<ITimer> stoppingTimer);

    void push(uint8_t channel, const IAudioMeter::InterleavedSampleData& sampleData, const std::optional<TrackId>& trackId) override;
    void start(double sampleRate) override;
    void stop() override;

    muse::async::Channel<audioch_t, MeterSignal> dataChanged(const std::optional<TrackId>& trackId) override;

private:
    struct QueueSample
    {
        float peak = 0.f;
        float rms = 0.f;
    };

    struct QueueItem
    {
        TrackId trackId;
        audioch_t channel;
        QueueSample sample;
        TimePoint dacTime;
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

    using LevelMap = std::unordered_map<audioch_t, Levels>;

    struct TrackData {
        muse::async::Channel<audioch_t, MeterSignal> notificationChannel;
        LevelMap channelLevels;
    };

    static QueueSample getSamplesMaxValue(const float* buffer, size_t frames, size_t step);
    static void decay(LevelState&);
    static void maybeBumpUp(LevelState&, float newLinValue, int hangover);

    double m_sampleRate{ 44100.0 };
    std::atomic<int> m_hangoverCount = 0;
    int m_maxFramesPerPush = 0;
    LockFreeQueue<QueueItem> m_lockFreeQueue{ 1024 };
    std::queue<QueueItem> m_mainThreadQueue;
    std::map<TrackId, TrackData> m_trackData;
    const std::unique_ptr<ITimer> m_playingTimer;
    const std::unique_ptr<ITimer> m_stoppingTimer;
    std::atomic<bool> m_running { false };
    bool m_warningIssued = false;
};
}
