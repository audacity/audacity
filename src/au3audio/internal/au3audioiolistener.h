/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/timer.h"
#include "global/async/asyncable.h"
#include "global/async/notification.h"
#include "global/async/channel.h"

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
        });
    }

    void OnAudioIORate(int /*rate*/) override { }
    void OnAudioIOStartRecording() override { m_recordingUpdateTimer.start(); }
    void OnAudioIONewBlocks() override { }
    void OnCommitRecording() override { m_recordingCommitRequested.notify(); }
    void OnSoundActivationThreshold() override { }
    void OnAudioIOStopRecording() override
    {
        m_recordingFinished.notify();
        m_recordingUpdateTimer.stop();
    }

    muse::async::Notification recordingUpdateRequested() const { return m_recordingUpdateRequested; }
    muse::async::Notification recordingCommitRequested() const { return m_recordingCommitRequested; }
    muse::async::Notification recordingFinished() const { return m_recordingFinished; }

    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const { return m_recordingClipChanged; }

private:
    muse::async::Notification m_recordingUpdateRequested;
    muse::async::Notification m_recordingCommitRequested;
    muse::async::Notification m_recordingFinished;

    muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> m_recordingClipChanged;

    muse::Timer m_recordingUpdateTimer;
};
}
