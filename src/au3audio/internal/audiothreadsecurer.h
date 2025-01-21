/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <thread>

// from muse
#include "audio/iaudiothreadsecurer.h"

namespace au::audio {
class AudioThreadSecurer : public muse::audio::IAudioThreadSecurer
{
public:
    AudioThreadSecurer() = default;

    bool isMainThread() const override;
    std::thread::id mainThreadId() const override;
    bool isAudioWorkerThread() const override;
    std::thread::id workerThreadId() const override;

    void setupMainThread();
    void setupWorkerThread();

private:

    std::thread::id m_mainThreadID;
    std::thread::id m_workerThreadID;
};
}
