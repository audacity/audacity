/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"
#include "framework/global/iapplication.h"
#include "framework/global/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/ui/iuiactionsregister.h"

#include "audio/audiotypes.h"
#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackcontroller.h"
#include "playback/iplayer.h"
#include "record/irecordcontroller.h"
#include "trackedit/iselectioncontroller.h"

namespace au::playback {
class PlaybackUiActions;
class PlaybackController : public IPlaybackController, public muse::actions::Actionable, public muse::async::Asyncable, public muse::Injectable
{
public:
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;

    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider { this };
    muse::Inject<IPlayback> playback { this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<muse::IApplication> application { this };
    muse::Inject<muse::IInteractive> interactive { this };
    muse::Inject<record::IRecordController> recordController{ this };
    muse::Inject<trackedit::ISelectionController> selectionController{ this };

public:
    PlaybackController(const muse::modularity::ContextPtr& ctx) : muse::Injectable(ctx) {}

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
    void stopSeekAndUpdatePlaybackRegion() override;

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

    muse::Progress loadingProgress() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    friend class PlaybackControllerTests;

    IPlayerPtr player() const;

    bool isLoaded() const;

    bool loopBoundariesSet() const;

    PlaybackRegion selectionPlaybackRegion() const;
    bool isSelectionPlaybackRegionChanged() const;
    void updatePlaybackRegion();

    void onProjectChanged();
    void onPlaybackPositionChanged();

    void onSelectionChanged();
    void seekListSelection();
    void seekRangeSelection();

    void togglePlayAction();
    void doPlay(bool ignoreSelection);
    void stopAction();
    void playTracksAction(const muse::actions::ActionQuery& q);
    void rewindToStartAction();
    void rewindToEndAction();
    void onSeekAction(const muse::actions::ActionQuery& q);
    void doSeek(const muse::secs_t secs, bool applyIfPlaying);
    void onChangePlaybackRegionAction(const muse::actions::ActionQuery& q);
    void doChangePlaybackRegion(const PlaybackRegion& region);
    void pauseAction();
    void doPause();
    void doResume();
    void seek(const muse::secs_t secs, bool applyIfPlaying);

    void togglePlayRepeats();
    void toggleAutomaticallyPan();

    void setLoopRegionToSelection();
    void setSelectionToLoop();
    void setLoopRegionInOut();
    void setSelectionFollowsLoopRegion();

    void openPlaybackSetupDialog();

    void setAudioApi(const muse::actions::ActionQuery& q);
    void setAudioOutputDevice(const muse::actions::ActionQuery& q);
    void setAudioInputDevice(const muse::actions::ActionQuery& q);
    void setInputChannels(const muse::actions::ActionQuery& q);
    void rescanAudioDevices();

    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void subscribeOnAudioParamsChanges();
    void setupSequenceTracks();
    void setupSequencePlayer();

    void initMuteStates();

    void updateSoloMuteStates();
    void updateAuxMuteStates();

    bool isEqualToPlaybackPosition(muse::secs_t position) const;
    bool isPlaybackPositionOnTheEndOfProject() const;
    bool isPlaybackPositionOnTheEndOfPlaybackRegion() const;
    bool isPlaybackStartPositionValid() const;
    bool isSeekPositionValid(const muse::secs_t& seekTime) const;
    muse::secs_t playbackPosition() const;

    using TrackAddFinished = std::function<void ()>;

    playback::IPlayerPtr m_player;

    muse::async::Notification m_isPlayAllowedChanged;
    muse::async::Notification m_isPlayingChanged;
    muse::async::Notification m_totalPlayTimeChanged;
    muse::async::Notification m_currentTempoChanged;
    muse::async::Channel<uint32_t> m_tickPlayed;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    muse::async::Notification m_currentSequenceIdChanged;
    muse::secs_t m_lastPlaybackSeekTime = 0.0;
    PlaybackRegion m_lastPlaybackRegion;

    muse::async::Channel<playback::TrackId> m_trackAdded;
    muse::async::Channel<playback::TrackId> m_trackRemoved;

    muse::async::Channel<audio::aux_channel_idx_t, std::string> m_auxChannelNameChanged;

    muse::Progress m_loadingProgress;
    size_t m_loadingTrackCount = 0;

    bool m_isExportingAudio = false;
    bool m_isRangeSelection = false;
};
}
