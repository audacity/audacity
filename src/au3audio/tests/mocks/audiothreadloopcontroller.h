/*
 * Audacity: A Digital Audio Editor
 */
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <string>

#include "au3-audio-io/AudioIO.h"

namespace au::au3audio::test {
/**
 * @brief Allows control of the AudioThread loop iterations.
 */
class AudioThreadLoopController final : public AudioIoCallback::AudioThreadPacer
{
public:
    AudioThreadLoopController(int maxLoopIterations, int audioThreadIterationsPerMainThreadWait = 1)
        : m_maxLoopIterations{maxLoopIterations}, m_audioThreadIterationsPerMainThreadWait{audioThreadIterationsPerMainThreadWait} {}

    //! NOTE: Must be called before AudioIO::Deinit, which joins the (otherwise blocked) thread.
    void release()
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_released = true;
        m_cv.notify_all();
    }

    bool waitGaveUp() const { return m_completed >= m_maxLoopIterations; }

    // Called by production code.
private:
    // ... from AudioThread
    void SleepUntil(const std::chrono::steady_clock::time_point&) override
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_cv.wait(lock, [this] {
            return m_released || m_allowed > m_completed;
        });
        ++m_completed;
        m_cv.notify_all();
    }

    // ... from Main thread
    void SleepFor(const std::chrono::milliseconds&) override
    {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_allowed += m_audioThreadIterationsPerMainThreadWait;
        m_cv.notify_all();
        m_cv.wait(lock, [this] {
            return m_completed >= m_allowed;
        });
    }

    // ... from Main thread
    bool KeepWaiting() const override
    {
        return m_completed < m_maxLoopIterations;
    }

private:
    std::mutex m_mutex;
    std::condition_variable m_cv;

    const int m_maxLoopIterations;
    const int m_audioThreadIterationsPerMainThreadWait;
    int m_allowed = 0;
    int m_completed = 0;
    bool m_released = false;
};
}
