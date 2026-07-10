/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <functional>
#include <optional>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "playback/iplayer.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/itransport.h"
#include "record/irecord.h"
#include "record/irecordcontroller.h"
#include "trackedit/iselectioncontroller.h"

namespace au::playback {
class Transport : public ITransport, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<IPlayer> player { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<au::record::IRecord> record { this };
    muse::ContextInject<record::IRecordController> recordController { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<audio::IAudioDevicesProvider> audioDevicesProvider { this };

public:
    Transport(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    bool isPlayAllowed() const override;
    muse::async::Notification isPlayAllowedChanged() const override;

    void togglePlay(bool ignoreSelection) override;
    void pause() override;
    void stop() override;
    void seekTo(muse::secs_t secs, bool triggerPlay) override;
    void rewindToStart() override;
    void rewindToEnd() override;
    void changePlaybackRegion(muse::secs_t start, muse::secs_t end) override;
    void stopSeekAndUpdatePlaybackRegion() override;

    muse::secs_t lastPlaybackSeekTime() const override;
    void setLastPlaybackSeekTime(muse::secs_t secs) override;
    muse::async::Notification lastPlaybackSeekTimeChanged() const override;

    void toggleLoopPlayback() override;
    void setLoopRegionToSelection() override;
    void setSelectionToLoop() override;
    void setLoopRegionInOut() override;
    void setSelectionFollowsLoopRegion() override;

    void setAudioApi(const std::string& api) override;
    void setAudioOutputDevice(const std::string& device) override;
    void setAudioInputDevice(const std::string& device) override;
    void setInputChannels(int channels) override;
    void setDefaultSampleRate(uint64_t rate) override;
    void setBufferLength(double duration) override;
    void rescanAudioDevices() override;

private:
    friend class TransportTests;

    muse::secs_t totalPlayTime() const;

    bool isPlaying() const;
    bool isPaused() const;
    bool isStopped() const;

    void doPlay(bool ignoreSelection);
    void doSeek(muse::secs_t secs, bool applyIfPlaying);
    void doChangePlaybackRegion(const PlaybackRegion& region);
    void seek(muse::secs_t secs, bool applyIfPlaying);

    //! Applies `change` while the audio stream is inactive: stop, apply, and
    //! resume from the same position if the user was actively playing (or
    //! remember it for the next play if they were paused).
    void withStreamRestart(const std::function<void()>& change);

    PlaybackRegion selectionPlaybackRegion() const;
    bool isSelectionPlaybackRegionChanged() const;
    void updatePlaybackRegion();
    void onProjectChanged();
    void onPlaybackPositionChanged();

    bool isEqualToPlaybackPosition(muse::secs_t position) const;
    bool isPlaybackPositionOnTheEndOfProject() const;
    bool isPlaybackPositionOnTheEndOfPlaybackRegion() const;
    bool isPlaybackStartPositionValid() const;
    bool isSeekPositionValid(const muse::secs_t& seekTime) const;

    muse::async::Notification m_isPlayAllowedChanged;
    muse::async::Notification m_lastPlaybackSeekTimeChanged;

    muse::secs_t m_lastPlaybackSeekTime = 0.0;
    PlaybackRegion m_lastPlaybackRegion;
    bool m_pauseShouldStopPlayback = false;

    //! Set when a device change tears down a paused stream: where the next play
    //! resumes from. Separate from m_lastPlaybackSeekTime (the stop anchor).
    std::optional<muse::secs_t> m_pausedResumePos;
};
}
