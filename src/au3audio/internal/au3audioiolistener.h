/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>

#include "framework/global/timer.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/notification.h"
#include "framework/global/async/channel.h"

#include "au3-audio-io/AudioIOListener.h"

#include "au3wrap/au3types.h"

namespace au::au3audio {
class Au3AudioIOListener : public AudioIOListener, public muse::async::Asyncable
{
public:
    Au3AudioIOListener()
        : m_recordingUpdateTimer(std::chrono::milliseconds(100))
    {
        m_recordingUpdateTimer.onTimeout(this, [this]() {
            m_recordingUpdateRequested.notify();
            if (m_newBlocks.exchange(false)) {
                m_recordingNewBlocks.notify();
            }
        });
    }

    // Called from the main thread
    void OnAudioIORate(int /*rate*/) override { }
    void OnAudioIOStartRecording() override { m_recordingUpdateTimer.start(); }
    void OnCommitRecording() override { m_recordingCommitRequested.notify(); }
    void OnAudioIOStopRecording() override
    {
        m_recordingFinished.notify();
        m_recordingUpdateTimer.stop();
    }

    // Called from audio thread
    void OnAudioIONewBlocks() override { m_newBlocks = true; }

    // Called from the hardware thread
    void OnSoundActivationThreshold() override { }

    muse::async::Notification recordingUpdateRequested() const { return m_recordingUpdateRequested; }
    muse::async::Notification recordingNewBlocks() const { return m_recordingNewBlocks; }
    muse::async::Notification recordingCommitRequested() const { return m_recordingCommitRequested; }
    muse::async::Notification recordingFinished() const { return m_recordingFinished; }

    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const { return m_recordingClipChanged; }

private:
    muse::async::Notification m_recordingUpdateRequested;
    muse::async::Notification m_recordingNewBlocks;
    muse::async::Notification m_recordingCommitRequested;
    muse::async::Notification m_recordingFinished;

    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> m_recordingClipChanged;

    muse::Timer m_recordingUpdateTimer;
    std::atomic<bool> m_newBlocks = false;
};
}
