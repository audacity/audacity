/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/ui/iuiactionsregister.h"
#include "framework/toast/itoastservice.h"

#include "audio/audiotypes.h"
#include "audio/driver/iaudiodrivercontroller.h"
#include "audio/iaudioengine.h"
#include "audio/iaudiostreamsuspender.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackcontroller.h"
#include "playback/iplayer.h"
#include "playback/itrackplaybackcontrol.h"
#include "record/irecordcontroller.h"
#include "record/irecord.h"
#include "trackedit/internal/itracknavigationcontroller.h"
#include "trackedit/iselectioncontroller.h"

namespace au::playback {
class PlaybackUiActions;
class PlaybackController : public IPlaybackController, public audio::IAudioStreamSuspender, public muse::actions::Actionable,
    public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
public:
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;
    muse::GlobalInject<audio::IAudioDriverController> audioDriverController;
    muse::GlobalInject<audio::IAudioEngine> audioEngine;
    muse::GlobalInject<muse::toast::IToastService> toastService;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<IPlayback> playback { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<record::IRecordController> recordController{ this };
    muse::ContextInject<record::IRecord> record{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController{ this };
    muse::ContextInject<ITrackPlaybackControl> trackPlaybackControl{ this };

public:
    PlaybackController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void deinit();

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

    muse::async::Channel<uint32_t> midiTickPlayed() const override;

    muse::async::Channel<playback::TrackId> trackAdded() const override;
    muse::async::Channel<playback::TrackId> trackRemoved() const override;

    // ISoloMuteState::SoloMuteState trackSoloMuteState(const TrackId& trackId) const override;
    // void setTrackSoloMuteState(const TrackId& trackId,
    //                            const ISoloMuteState::SoloMuteState& state) const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;

    muse::secs_t totalPlayTime() const override;
    muse::async::Notification totalPlayTimeChanged() const override;
    muse::secs_t lastPlaybackSeekTime() const override;
    void setLastPlaybackSeekTime(muse::secs_t secs) override;
    muse::async::Notification lastPlaybackSeekTimeChanged() const override;

    audio::AudioStreamRestorer suspendForAudioConfiguration(
        audio::AudioStreamKind streamKind) override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    friend class PlaybackControllerTests;

    IPlayerPtr player() const;

    bool loopBoundariesSet() const;

    PlaybackRegion selectionPlaybackRegion() const;

    void onProjectChanged();
    void onPlaybackPositionChanged();

    void onSelectionChanged();
    void seekListSelection();
    void seekRangeSelection();

    enum class TogglePlayMode {
        PlayPause,      //!< pause while playing; resume/replay when not
        PlayStop,       //!< stop while playing; play when not
        PlayStopAndSetCursor, //!< stop and seek to the stop position while playing, so the next play continues from there; play when not
    };

    muse::Ret togglePlay(TogglePlayMode mode);

    void stopAndSeekToLastSeekTime();
    void stopAndSeekToPlaybackPosition();

    muse::Ret togglePlayPauseAction();
    muse::Ret togglePlayStopAction();
    muse::Ret togglePlayStopAndSetCursorAction();
    muse::Ret playSelectionAction();
    void doPlay();
    muse::Ret stopAction();
    muse::Ret playTracksAction(const muse::rcommand::Params& params);
    muse::Ret rewindToStartAction();
    muse::Ret rewindToEndAction();
    muse::Ret onSeekAction(const muse::rcommand::Params& params);
    void doSeek(const muse::secs_t secs, bool applyIfPlaying);
    muse::Ret onChangePlaybackRegionAction(const muse::rcommand::Params& params);
    void doChangePlaybackRegion(const PlaybackRegion& region);
    muse::Ret pauseAction();
    void doPause();
    void doResume();
    bool ensurePhysicalStreamStopped();

    muse::Ret togglePlayRepeats();
    muse::Ret toggleAutomaticallyPan();
    muse::Ret toggleMuteFocusedTrack();
    muse::Ret toggleSoloFocusedTrack();
    muse::Ret muteAllTracks();
    muse::Ret unmuteAllTracks();
    muse::Ret muteSelectedTracks();
    muse::Ret unmuteSelectedTracks();

    muse::Ret setLoopRegionToSelection();
    muse::Ret setSelectionToLoop();
    muse::Ret setLoopRegionInOut();
    muse::Ret setSelectionFollowsLoopRegion();

    void openPlaybackSetupDialog();

    muse::Ret setAudioApi(const muse::rcommand::Params& params);
    muse::Ret setAudioOutputDevice(const muse::rcommand::Params& params);
    muse::Ret setAudioInputDevice(const muse::rcommand::Params& params);
    muse::Ret setInputChannels(const muse::rcommand::Params& params);
    muse::Ret rescanAudioDevices();
    muse::Ret handleAudioConfigurationResult(const audio::ApplyResult& result, const muse::actions::ActionCode& actionCode);

    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);
    void subscribeOnAudioParamsChanges();
    void setupSequenceTracks();
    void setupSequencePlayer();

    void initMuteStates();

    void updateSoloMuteStates();
    void updateAuxMuteStates();

    bool isEqualToPlaybackPosition(muse::secs_t position) const;
    bool isPlaybackPositionOnTheEndOfProject() const;
    bool isPlaybackPositionAtOrAfterPlaybackRegionEnd() const;
    bool isPlaybackStartPositionValid() const;
    bool isSeekPositionValid(const muse::secs_t& seekTime) const;
    muse::secs_t playbackPosition() const;

    using TrackAddFinished = std::function<void ()>;

    playback::IPlayerPtr m_player;

    muse::async::Notification m_isPlayAllowedChanged;
    muse::async::Notification m_isPlayingChanged;
    muse::async::Notification m_totalPlayTimeChanged;
    muse::async::Notification m_lastPlaybackSeekTimeChanged;
    muse::async::Notification m_loopRegionChanged;
    muse::async::Notification m_currentTempoChanged;
    muse::async::Channel<uint32_t> m_tickPlayed;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    muse::async::Notification m_currentSequenceIdChanged;
    muse::secs_t m_lastPlaybackSeekTime = 0.0;
    bool m_pauseShouldStopPlayback = false;
    bool m_isPlayingSelection = false;
    std::optional<muse::secs_t> m_pausedResumePos;

    muse::async::Channel<playback::TrackId> m_trackAdded;
    muse::async::Channel<playback::TrackId> m_trackRemoved;
};
}
