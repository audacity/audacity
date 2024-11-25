/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"
#include "iapplication.h"
#include "../iplayback.h"
#include "record/irecordcontroller.h"
#include "trackedit/iselectioncontroller.h"

#include "au3audio/audiotypes.h"
#include "../iplayer.h"
#include "../iplaybackcontroller.h"

namespace au::playback {
class PlaybackController : public IPlaybackController, public muse::actions::Actionable, public muse::async::Asyncable
{
public:
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<muse::IApplication> application;
    muse::Inject<IPlayback> playback;
    muse::Inject<record::IRecordController> recordController;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    void init();
    void deinit();

    bool isPlayAllowed() const override;
    muse::async::Notification isPlayAllowedChanged() const override;

    bool isPlaying() const override;
    muse::async::Notification isPlayingChanged() const override;

    bool isPaused() const override;

    void reset() override;

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

    bool isStoped() const;

    bool isLoopEnabled() const;
    bool loopBoundariesSet() const;

    PlaybackRegion selectionPlaybackRegion() const;
    bool isSelectionPlaybackRegionChanged() const;

    void onProjectChanged();

    void onSelectionChanged();
    void seekListSelection();
    void seekRangeSelection();

    void togglePlay();
    void rewindToStart();
    void rewindToEnd();
    void onSeekAction(const muse::actions::ActionData& args);
    void doSeek(const muse::secs_t secs, bool applyIfPlaying = false);
    void onChangePlaybackRegionAction(const muse::actions::ActionData& args);
    void doChangePlaybackRegion(const PlaybackRegion& region);
    void play(bool ignoreSelection = false);
    void pause();
    void stop();
    void resume();
    void seek(const muse::secs_t secs);

    void togglePlayRepeats();
    void toggleAutomaticallyPan();
    void toggleLoopPlayback();

    void openPlaybackSetupDialog();

    // void addLoopBoundary(LoopBoundaryType type);
    // void addLoopBoundaryToTick(LoopBoundaryType type, int tick);
    // void updateLoop();

    void enableLoop();
    void disableLoop();

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
