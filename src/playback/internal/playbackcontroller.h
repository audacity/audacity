/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/iapplication.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackcontroller.h"
#include "playback/iplayer.h"
#include "record/irecordcontroller.h"

namespace au::playback {
class PlaybackController : public IPlaybackController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Contextable
{
public:
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;
    muse::GlobalInject<muse::IApplication> application;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<audio::IAudioDevicesProvider> audioDevicesProvider { this };
    muse::ContextInject<IPlayback> playback { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<record::IRecordController> recordController{ this };

public:
    PlaybackController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void deinit();

    //! IPlaybackController is now a thin pass-through: the playback session state
    //! and logic live on the player. These forward to it so the existing
    //! consumers keep working unchanged; the next commit points them straight at
    //! the player and drops this interface.
    bool isPlayAllowed() const override;
    muse::async::Notification isPlayAllowedChanged() const override;

    bool isPlaying() const override;
    muse::async::Notification isPlayingChanged() const override;
    PlaybackStatus playbackStatus() const override;

    bool isLoopRegionActive() const override;
    void toggleLoopPlayback() override;
    PlaybackRegion loopRegion() const override;
    void setLoopRegion(const PlaybackRegion& region) override;
    void setLoopRegionStart(const muse::secs_t time) override;
    void setLoopRegionEnd(const muse::secs_t time) override;
    void setLoopRegionActive(const bool active) override;
    void clearLoopRegion() override;
    void loopEditingBegin() override;
    void loopEditingEnd() override;
    bool isLoopRegionClear() const override;
    muse::async::Notification loopRegionChanged() const override;

    bool isPaused() const override;
    bool isStopped() const override;

    void stop() override;
    void stopSeekAndUpdatePlaybackRegion() override;

    muse::async::Channel<uint32_t> midiTickPlayed() const override;

    muse::async::Channel<playback::TrackId> trackAdded() const override;
    muse::async::Channel<playback::TrackId> trackRemoved() const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;

    muse::secs_t totalPlayTime() const override;
    muse::async::Notification totalPlayTimeChanged() const override;
    muse::secs_t lastPlaybackSeekTime() const override;
    void setLastPlaybackSeekTime(muse::secs_t secs) override;
    muse::async::Notification lastPlaybackSeekTimeChanged() const override;

    muse::Progress loadingProgress() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    friend class PlaybackControllerTests;

    void togglePlayAction();
    void stopAction();
    void playTracksAction(const muse::actions::ActionQuery& q);
    void rewindToStartAction();
    void rewindToEndAction();
    void onSeekAction(const muse::actions::ActionQuery& q);
    void onChangePlaybackRegionAction(const muse::actions::ActionQuery& q);
    void pauseAction();

    void togglePlayRepeats();
    void toggleAutomaticallyPan();

    void setLoopRegionToSelection();
    void setSelectionToLoop();
    void setLoopRegionInOut();
    void setSelectionFollowsLoopRegion();

    void setAudioApi(const muse::actions::ActionQuery& q);
    void setAudioOutputDevice(const muse::actions::ActionQuery& q);
    void setAudioInputDevice(const muse::actions::ActionQuery& q);
    void setInputChannels(const muse::actions::ActionQuery& q);
    void rescanAudioDevices();

    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    playback::IPlayerPtr m_player;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    // Dead members, kept only to satisfy the IPlaybackController interface; they
    // were never populated and have no player equivalent. Removed together with
    // IPlaybackController in the follow-up commit.
    muse::async::Channel<uint32_t> m_tickPlayed;
    muse::async::Channel<playback::TrackId> m_trackAdded;
    muse::async::Channel<playback::TrackId> m_trackRemoved;
    muse::async::Notification m_totalPlayTimeChanged;
    muse::Progress m_loadingProgress;
};
}
