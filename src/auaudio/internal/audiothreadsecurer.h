/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <thread>

// from muse
#include "audio/common/iaudiothreadsecurer.h"

namespace au::auaudio {
class AudioThreadSecurer : public muse::audio::IAudioThreadSecurer
{
public:
    AudioThreadSecurer() = default;

    bool isMainThread() const override;
    std::thread::id mainThreadId() const override;
    bool isAudioEngineThread() const override;
    std::thread::id audioEngineThreadId() const override;

    void setupMainThread();
    void setupAudioEngineThread();

private:

    std::thread::id m_mainThreadID;
    std::thread::id m_audioEngineThreadID;
};
}
